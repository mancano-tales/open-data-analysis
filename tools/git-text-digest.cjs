// Hash UTF-8/text bytes as Git text, normalizing CRLF only. All other bytes,
// including lone CRs and multibyte characters, remain significant.
'use strict';
const {createHash} = require('node:crypto');
const LF = Buffer.from('\n'), CR = Buffer.from('\r');
async function gitTextDigest(stream) {
  const hash = createHash('sha256');
  let pendingCR = false;
  for await (let chunk of stream) {
    if (!Buffer.isBuffer(chunk)) chunk = Buffer.from(chunk);
    if (!chunk.length) continue;
    if (pendingCR) {
      const paired = chunk[0] === 10;
      hash.update(paired ? LF : CR);
      if (paired) chunk = chunk.subarray(1);
      pendingCR = false;
    }
    if (chunk.at(-1) === 13) {
      pendingCR = true;
      chunk = chunk.subarray(0, -1);
    }
    let offset = 0, boundary;
    while ((boundary = chunk.indexOf('\r\n', offset)) !== -1) {
      hash.update(chunk.subarray(offset, boundary));
      hash.update(LF);
      offset = boundary + 2;
    }
    hash.update(chunk.subarray(offset));
  }
  if (pendingCR) hash.update(CR);
  return hash.digest('hex');
}
module.exports = {gitTextDigest};
