#!/usr/bin/env node
/* Render first, publish second. Raw source aggregates are never modified. */
'use strict';
const fs=require('node:fs/promises'),nativeFs=require('node:fs'),path=require('node:path');
const zlib=require('node:zlib'),crypto=require('node:crypto');
const {pipeline}=require('node:stream/promises'),{execFileSync}=require('node:child_process');
const relativeSource='posts/explorador-eleitoral/data';
const relativePublished='docs/posts/explorador-eleitoral/data';
const relativePortuguese='docs/pt/posts/explorador-eleitoral/data';
function below(root,relative){if(path.isAbsolute(relative))throw Error('Absolute path rejected');const target=path.resolve(root,relative);if(!target.startsWith(root+path.sep))throw Error('Path escapes intended directory: '+relative);return target;}
async function exists(file){try{await fs.access(file);return true;}catch{return false;}}
async function atomicText(file,text){await fs.mkdir(path.dirname(file),{recursive:true});await fs.writeFile(file+'.part',text);await fs.rename(file+'.part',file);}
async function blobHash(file){const stat=await fs.stat(file),hash=crypto.createHash('sha1');hash.update(`blob ${stat.size}\0`);for await(const chunk of nativeFs.createReadStream(file))hash.update(chunk);return hash.digest('hex');}
function git(root,args){return execFileSync('git',args,{cwd:root,encoding:'utf8',maxBuffer:32*1024*1024});}
function headBlobs(root){const result=new Map();for(const record of git(root,['ls-tree','-rz','--full-tree','HEAD','--',relativeSource,relativePublished,relativePortuguese]).split('\0')){if(!record)continue;const [header,file]=record.split('\t');const [mode,type,hash]=header.split(' ');if(type==='blob'&&mode!=='120000')result.set(file,hash);}return result;}
async function trackedPortuguese(root){return git(root,['ls-files','-z','--',relativePortuguese]).split('\0').filter(Boolean).map(relative=>({file:below(root,relative),relative})).filter(item=>/\.(json|csv)$/i.test(item.relative));}
async function verifyBackup(root,deletions){
  let blobs;try{blobs=headBlobs(root);}catch{throw Error('Pruning requires a valid HEAD containing the original data backup.');}
  for(const item of deletions){
    const hash=await blobHash(item.file);
    const suffix=item.relative.startsWith(relativePortuguese+'/')?item.relative.slice(relativePortuguese.length+1):item.relative.slice(relativePublished.length+1);
    const candidates=[item.relative,relativeSource+'/'+suffix,relativePublished+'/'+suffix];
    if(!candidates.some(candidate=>blobs.get(candidate)===hash))throw Error('Pruning refused: no byte-identical original preserved in HEAD for '+item.relative);
  }
}
async function publish({repoRoot=path.resolve(__dirname,'../..'),apply=false,pruneJson=false,prunePt=false}={}){
  repoRoot=path.resolve(repoRoot);
  if((pruneJson||prunePt)&&!apply)throw Error('Pruning flags require --apply; dry-run never removes files.');
  const sourceRoot=below(repoRoot,relativeSource),publishedRoot=below(repoRoot,relativePublished);
  const manifest=JSON.parse(await fs.readFile(below(sourceRoot,'manifest.json'),'utf8'));
  const transformed={...manifest,series:manifest.series.map(entry=>({...entry,url:entry.url+'.gz'}))};
  const files=[];
  for(const entry of manifest.series){
    if(!entry.url.endsWith('.json'))throw Error('Expected uncompressed source JSON URL: '+entry.url);
    const source=below(sourceRoot,entry.url),destination=below(publishedRoot,entry.url+'.gz');
    const stat=await fs.lstat(source);if(!stat.isFile()||stat.isSymbolicLink())throw Error('Source must be a regular file: '+entry.url);
    files.push({source,destination,size:stat.size,relative:relativePublished+'/'+entry.url});
  }
  const deletions=[];
  if(pruneJson)for(const file of files){const original=below(repoRoot,file.relative);if(await exists(original))deletions.push({file:original,relative:file.relative});}
  if(prunePt)for(const item of await trackedPortuguese(repoRoot))if(await exists(item.file))deletions.push(item);
  // Validate every exact target and its durable Git backup before changing any file.
  if(deletions.length)await verifyBackup(repoRoot,deletions);
  const sourceBytes=files.reduce((sum,file)=>sum+file.size,0);
  const removedBytes=(await Promise.all(deletions.map(item=>fs.stat(item.file)))).reduce((sum,stat)=>sum+stat.size,0);
  if(!apply)return {mode:'dry-run',series:files.length,sourceBytes,publishedRoot:relativePublished,plannedFormat:'json.gz',deletions:0};
  let compressedBytes=0;
  for(const file of files){
    await fs.mkdir(path.dirname(file.destination),{recursive:true});
    const before=await fs.stat(file.source);
    try{
      // Node's gzip header has zero mtime and no source filename: deterministic bytes.
      await pipeline(nativeFs.createReadStream(file.source),zlib.createGzip({level:9}),nativeFs.createWriteStream(file.destination+'.part'));
      const after=await fs.stat(file.source);
      if(before.size!==after.size||before.mtimeMs!==after.mtimeMs)throw Error('Source changed during publication: '+file.source);
      await fs.rename(file.destination+'.part',file.destination);
    }catch(error){await fs.unlink(file.destination+'.part').catch(()=>{});throw error;}
    compressedBytes+=(await fs.stat(file.destination)).size;
  }
  if(await exists(below(sourceRoot,'availability.csv')))await fs.copyFile(below(sourceRoot,'availability.csv'),below(publishedRoot,'availability.csv'));
  // Switch the manifest only after all compressed series are ready.
  await atomicText(below(publishedRoot,'manifest.json'),JSON.stringify(transformed)+'\n');
  // No recursive removal: these are the individually validated targets above.
  for(const item of deletions)await fs.unlink(item.file);
  return {mode:'published',series:files.length,sourceBytes,compressedBytes,ratio:compressedBytes/sourceBytes,removedFiles:deletions.length,removedBytes,publishedRoot:relativePublished};
}
module.exports={publish};
if(require.main===module){
  const allowed=new Set(['--apply','--prune-json','--prune-pt']);
  for(const argument of process.argv.slice(2))if(!allowed.has(argument))throw Error('Unknown option: '+argument);
  publish({apply:process.argv.includes('--apply'),pruneJson:process.argv.includes('--prune-json'),prunePt:process.argv.includes('--prune-pt')}).then(result=>console.log(JSON.stringify(result,null,2))).catch(error=>{console.error(error.message);process.exitCode=1;});
}
