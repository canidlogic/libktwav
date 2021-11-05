/*
 * wav.c
 * =====
 * 
 * Implementation of wav.h
 * 
 * See the header for further information.
 */

#include "wav.h"

#include <stdlib.h>
#include <string.h>

/*
 * Constants
 * =========
 */

/*
 * The maximum length of RIFF files in bytes.
 * 
 * This is set to one gigabyte.  Anything above may have odd behavior.
 */
#define MAX_RIFF_LEN INT32_C(1073741824)

/*
 * The minimum size in bytes of the WAVE header.
 */
#define HEAD_MINSIZE (16)

/*
 * The full output WAV header size in bytes.
 */
#define FULLHEAD_SIZE (44)

/*
 * RIFF FOURCC codes.
 */
#define FOURCC_RIFF UINT32_C(0x52494646)    /* RIFF */
#define FOURCC_WAVE UINT32_C(0x57415645)    /* WAVE */
#define FOURCC_FMT  UINT32_C(0x666d7420)    /* fmt  */
#define FOURCC_DATA UINT32_C(0x64617461)    /* data */

/*
 * Local functions
 * ===============
 */

/* Prototypes */
static uint32_t readInt32BE(FILE *pIn, int *status);
static uint32_t readInt32LE(FILE *pIn, int *status);
static uint16_t readInt16LE(FILE *pIn, int *status);

static int32_t seekChunk(
    FILE     * fp,
    uint32_t   riff_type,
    uint32_t   ck_type);

static void writeInt32BE(FILE *pOut, uint32_t i);
static void writeInt32LE(FILE *pOut, uint32_t i);
static void writeInt16LE(FILE *pOut, uint16_t i);

/*
 * Read a 32-bit big-endian integer from a given file.
 * 
 * pIn is the file to read from.  It must be open for reading or
 * undefined behavior occurs.  Reading is fully sequential.
 * 
 * Four bytes are read from the current file position to form an
 * integer in big endian order (most significant byte first).
 * 
 * If the read operation fails, zero is returned and *status is set to
 * zero.  status must be checked to distinguish between reading an
 * integer value zero and an error!
 * 
 * If *status is zero upon entry, the function fails without doing
 * anything further.
 * 
 * Parameters:
 * 
 *   pIn - the file to read from
 * 
 *   status - pointer to the flag to clear if failure
 * 
 * Return:
 * 
 *   the integer value that was read from the file, zero means either
 *   successful read of zero or error
 */
static uint32_t readInt32BE(FILE *pIn, int *status) {
  
  uint32_t val = 0;
  int b1 = 0;
  int b2 = 0;
  int b3 = 0;
  int b4 = 0;
  
  /* Check parameters */
  if ((pIn == NULL) || (status == NULL)) {
    abort();
  }
  
  /* Read four bytes in big endian order */
  if (*status) {
    b1 = getc(pIn);
    if (b1 == EOF) {
      *status = 0;
    }
  }
  if (*status) {
    b2 = getc(pIn);
    if (b2 == EOF) {
      *status = 0;
    }
  }
  if (*status) {
    b3 = getc(pIn);
    if (b3 == EOF) {
      *status = 0;
    }
  }
  if (*status) {
    b4 = getc(pIn);
    if (b4 == EOF) {
      *status = 0;
    }
  }
  
  /* Form the value */
  if (*status) {
    val = (((uint32_t) b1) << 24);
    val |= (((uint32_t) b2) << 16);
    val |= (((uint32_t) b3) << 8);
    val |= ((uint32_t) b4);
  }
  
  /* Return value or zero */
  return val;
}

/*
 * Read a 32-bit little-endian integer from a given file.
 * 
 * pIn is the file to read from.  It must be open for reading or
 * undefined behavior occurs.  Reading is fully sequential.
 * 
 * Four bytes are read from the current file position to form an
 * integer in little endian order (least significant byte first).
 * 
 * If the read operation fails, zero is returned and *status is set to
 * zero.  status must be checked to distinguish between reading an
 * integer value zero and an error!
 * 
 * If *status is zero upon entry, the function fails without doing
 * anything further.
 * 
 * Parameters:
 * 
 *   pIn - the file to read from
 * 
 *   status - pointer to the flag to clear if failure
 * 
 * Return:
 * 
 *   the integer value that was read from the file, zero means either
 *   successful read of zero or error
 */
static uint32_t readInt32LE(FILE *pIn, int *status) {
  
  uint32_t val = 0;
  int b1 = 0;
  int b2 = 0;
  int b3 = 0;
  int b4 = 0;
  
  /* Check parameters */
  if ((pIn == NULL) || (status == NULL)) {
    abort();
  }
  
  /* Read four bytes in little endian order */
  if (*status) {
    b4 = getc(pIn);
    if (b4 == EOF) {
      *status = 0;
    }
  }
  if (*status) {
    b3 = getc(pIn);
    if (b3 == EOF) {
      *status = 0;
    }
  }
  if (*status) {
    b2 = getc(pIn);
    if (b2 == EOF) {
      *status = 0;
    }
  }
  if (*status) {
    b1 = getc(pIn);
    if (b1 == EOF) {
      *status = 0;
    }
  }
  
  /* Form the value */
  if (*status) {
    val = (((uint32_t) b1) << 24);
    val |= (((uint32_t) b2) << 16);
    val |= (((uint32_t) b3) << 8);
    val |= ((uint32_t) b4);
  }
  
  /* Return value or zero */
  return val;
}

/*
 * Read a 16-bit little-endian integer from a given file.
 * 
 * pIn is the file to read from.  It must be open for reading or
 * undefined behavior occurs.  Reading is fully sequential.
 * 
 * Two bytes are read from the current file position to form an integer
 * in little endian order (least significant byte first).
 * 
 * If the read operation fails, zero is returned and *status is set to
 * zero.  status must be checked to distinguish between reading an
 * integer value zero and an error!
 * 
 * If *status is zero upon entry, the function fails without doing
 * anything further.
 * 
 * Parameters:
 * 
 *   pIn - the file to read from
 * 
 *   status - pointer to the flag to clear if failure
 * 
 * Return:
 * 
 *   the integer value that was read from the file, zero means either
 *   successful read of zero or error
 */
static uint16_t readInt16LE(FILE *pIn, int *status) {
  
  uint16_t val = 0;
  int b1 = 0;
  int b2 = 0;
  
  /* Check parameters */
  if ((pIn == NULL) || (status == NULL)) {
    abort();
  }
  
  /* Read two bytes in little endian order */
  if (*status) {
    b2 = getc(pIn);
    if (b2 == EOF) {
      *status = 0;
    }
  }
  if (*status) {
    b1 = getc(pIn);
    if (b1 == EOF) {
      *status = 0;
    }
  }
  
  /* Form the value */
  if (*status) {
    val = (((uint16_t) b1) << 8);
    val |= ((uint16_t) b2);
  }
  
  /* Return value or zero */
  return val;
}

/*
 * Seek to a specific chunk in a RIFF file.
 * 
 * fp is the RIFF file.  It must be open for reading and support random
 * access or undefined behavior occurs.  The file position on entry is
 * irrelevant.  The file position on a successful return will be right
 * at the start of the chunk data (or immediately after the chunk if the
 * chunk has no data).  The file position on a failed return is
 * undefined.
 * 
 * riff_type is the type of RIFF file.  The top-level RIFF chunk must
 * have a type matching this or the function fails.  For example, a WAV
 * file has 'WAVE' (0x57415645) here.  Unlike the rest of the RIFF file,
 * the RIFF type is stored in big endian (most significant byte first)
 * in the RIFF file.
 * 
 * ck_type is the type of chunk to look for.  Unlike the rest of the
 * RIFF file, chunk types are stored in big endian (most significant
 * byte first) in the RIFF file.
 * 
 * This function will only look through the chunks that are descendants
 * of the top-level RIFF node.  It will *not* recursively enter list
 * chunks.  It will also *not* look through subsequent top-level nodes
 * (this shouldn't happen in WAV files, but it sometimes does in AVI
 * files).
 * 
 * The function fails if the top-level RIFF chunk goes beyond the file
 * size limit of MAX_RIFF_LEN.
 * 
 * The function also fails if more than one chunk of the given type is
 * found.  A successful return therefore guarantees that there is only
 * one chunk of this type in the file.
 * 
 * The return value is the size in bytes of the data within this chunk,
 * which may be zero.  It will never exceed MAX_RIFF_LEN.  If the
 * function fails, the return value is -1.  On successful return, the
 * file pointer will be set to the first byte of the chunk data (or
 * immediately after the chunk if the chunk is empty and has no data).
 * 
 * Parameters:
 * 
 *   fp - the RIFF file
 * 
 *   riff_type - the RIFF file type
 * 
 *   ck_type - the chunk type to look for
 * 
 * Return:
 * 
 *   the size in bytes of the data within this chunk, or -1 if there was
 *   an error
 */
static int32_t seekChunk(
    FILE     * fp,
    uint32_t   riff_type,
    uint32_t   ck_type) {
  
  int status = 1;
  uint32_t top_size = 0;
  uint32_t cur_type = 0;
  uint32_t cur_len = 0;
  int32_t match_pos = -1;
  int32_t match_len = -1;
  
  /* Check parameters */
  if (fp == NULL) {
    abort();
  }
  
  /* Rewind to beginning of file */
  if (fseek(fp, 0, SEEK_SET)) {
    status = 0;
  }
  
  /* Read the RIFF signature */
  if (status) {
    if (readInt32BE(fp, &status) != FOURCC_RIFF) {
      status = 0;
    }
  }
  
  /* Read the size of the top-level RIFF chunk data */
  if (status) {
    top_size = readInt32LE(fp, &status);
  }
  
  /* The size of the top-level RIFF chunk data must be at least four
   * bytes (for the FOURCC type code), and at most (MAX_RIFF_LEN-8)
   * bytes (to avoid exceeding the file limit) */
  if (status) {
    if ((top_size < 4) || (top_size > MAX_RIFF_LEN - 8)) {
      status = 0;
    }
  }
  
  /* Read the type word and make sure it matches the given type */
  if (status) {
    if (readInt32BE(fp, &status) != riff_type) {
      status = 0;
    }
  }
  
  /* Subtract four from the size of the top-level data to account for
   * the RIFF type we just read */
  if (status) {
    top_size -= 4;
  }
  
  /* Read through all the chunks in this top-level block */
  while (status && (top_size > 0)) {
    
    /* We must have at least eight bytes remaining to allow for another
     * chunk header to be read */
    if (top_size < 8) {
      status = 0;
    }
    
    /* Read the current chunk type */
    if (status) {
      cur_type = readInt32BE(fp, &status);
    }
    
    /* If we have a matching chunk type, fail if we already found a
     * match (indicating a duplicate chunk) */
    if (status && (cur_type == ck_type)) {
      if (match_pos != -1) {
        status = 0;
      }
    }
    
    /* Read the current chunk length */
    if (status) {
      cur_len = readInt32LE(fp, &status);
    }
    
    /* Update top_size to account for the chunk header we just read */
    if (status) {
      top_size -= 8;
    }
    
    /* Make sure that chunk length does not exceed remaining space in
     * top-level chunk */
    if (status && (top_size < cur_len)) {
      status = 0;
    }
    
    /* If we have a match, store the match position and the match
     * length */
    if (status && (cur_type == ck_type)) {
      match_len = (int32_t) cur_len;
      match_pos = (int32_t) ftell(fp);
      if (match_pos == -1) {
        status = 0;
      }
    }
    
    /* If the current length is not even AND it is less than the
     * remaining bytes in the top-level chunk, increase it by one to
     * account for even-length padding */
    if (status && ((cur_len & 1) != 0)) {
      if (cur_len < top_size) {
        cur_len++;
      }
    }
    
    /* Skip over data and possible padding byte */
    if (status) {
      if (fseek(fp, (long) cur_len, SEEK_CUR)) {
        status = 0;
      }
      top_size -= cur_len;
    }
  }
  
  /* If we didn't get a match, fail */
  if (status && (match_pos == -1)) {
    status = 0;
  }
  
  /* Seek to the start of the match data */
  if (status) {
    if (fseek(fp, (long) match_pos, SEEK_SET)) {
      status = 0;
    }
  }
  
  /* If we failed, make sure we return -1 */
  if (!status) {
    match_len = -1;
  }
  
  /* Return size of chunk or -1 */
  return match_len;
}

/*
 * Write a 32-bit integer in big-endian order to the given file.
 * 
 * pOut is the file to write to.  It must be opened for writing or
 * undefined behavior occurs.  Writing is fully sequential.
 * 
 * i is the integer value to write.
 * 
 * Four bytes are written to output in big-endian order.
 * 
 * Parameters:
 * 
 *   pOut - the file to write to
 * 
 *   i - the integer value to write
 */
static void writeInt32BE(FILE *pOut, uint32_t i) {
  
  int b1 = 0;
  int b2 = 0;
  int b3 = 0;
  int b4 = 0;
  
  /* Check parameters */
  if (pOut == NULL) {
    abort();
  }
  
  /* Serialize into bytes */
  b1 = (int) ((i >> 24) & 0xff);
  b2 = (int) ((i >> 16) & 0xff);
  b3 = (int) ((i >> 8) & 0xff);
  b4 = (int) (i & 0xff);
  
  /* Write the bytes */
  if (putc(b1, pOut) == EOF) {
    abort();
  }
  if (putc(b2, pOut) == EOF) {
    abort();
  }
  if (putc(b3, pOut) == EOF) {
    abort();
  }
  if (putc(b4, pOut) == EOF) {
    abort();
  }
}

/*
 * Write a 32-bit integer in little-endian order to the given file.
 * 
 * pOut is the file to write to.  It must be opened for writing or
 * undefined behavior occurs.  Writing is fully sequential.
 * 
 * i is the integer value to write.
 * 
 * Four bytes are written to output in little-endian order.
 * 
 * Parameters:
 * 
 *   pOut - the file to write to
 * 
 *   i - the integer value to write
 */
static void writeInt32LE(FILE *pOut, uint32_t i) {
  
  int b1 = 0;
  int b2 = 0;
  int b3 = 0;
  int b4 = 0;
  
  /* Check parameters */
  if (pOut == NULL) {
    abort();
  }
  
  /* Serialize into bytes */
  b4 = (int) ((i >> 24) & 0xff);
  b3 = (int) ((i >> 16) & 0xff);
  b2 = (int) ((i >> 8) & 0xff);
  b1 = (int) (i & 0xff);
  
  /* Write the bytes */
  if (putc(b1, pOut) == EOF) {
    abort();
  }
  if (putc(b2, pOut) == EOF) {
    abort();
  }
  if (putc(b3, pOut) == EOF) {
    abort();
  }
  if (putc(b4, pOut) == EOF) {
    abort();
  }
}

/*
 * Write a 16-bit integer in little-endian order to the given file.
 * 
 * pOut is the file to write to.  It must be opened for writing or
 * undefined behavior occurs.  Writing is fully sequential.
 * 
 * i is the integer value to write.
 * 
 * Two bytes are written to output in little-endian order.
 * 
 * Parameters:
 * 
 *   pOut - the file to write to
 * 
 *   i - the integer value to write
 */
static void writeInt16LE(FILE *pOut, uint16_t i) {
  
  int b1 = 0;
  int b2 = 0;
  
  /* Check parameters */
  if (pOut == NULL) {
    abort();
  }
  
  /* Serialize into bytes */
  b2 = (int) ((i >> 8) & 0xff);
  b1 = (int) (i & 0xff);
  
  /* Write the bytes */
  if (putc(b1, pOut) == EOF) {
    abort();
  }
  if (putc(b2, pOut) == EOF) {
    abort();
  }
}

/*
 * Public function implementations
 * ===============================
 * 
 * See the header for specifications.
 */

/*
 * castDown function.
 */
int32_t castDown(int64_t v) {
  
  int32_t result = 0;
  
  /* Check parameter */
  if (v < 0) {
    abort();
  }
  
  /* Determine result */
  if (v <= INT32_MAX) {
    result = (int32_t) v;
  } else {
    result = -1;
  }
  
  /* Return result */
  return result;
}

/*
 * loadWAV function.
 */
WAVE_FILE *loadWAV(const char *pPath) {
  
  int status = 1;
  WAVE_FILE *pwv = NULL;
  int32_t head_len = 0;
  int32_t audio_len = 0;
  uint16_t ui16 = 0;
  uint32_t ui32 = 0;
  
  /* Check parameter */
  if (pPath == NULL) {
    abort();
  }
  
  /* Allocate a new WAVE_FILE structure */
  pwv = (WAVE_FILE *) malloc(sizeof(WAVE_FILE));
  if (pwv == NULL) {
    abort();
  }
  memset(pwv, 0, sizeof(WAVE_FILE));
  
  /* Initialize WAVE_FILE structure */
  pwv->fp = NULL;
  
  /* Open the WAVE file */
  pwv->fp = fopen(pPath, "rb");
  if (pwv->fp == NULL) {
    status = 0;
  }
  
  /* Seek to the WAVE header */
  if (status) {
    head_len = seekChunk(pwv->fp, FOURCC_WAVE, FOURCC_FMT);
    if (head_len < 0) {
      status = 0;
    }
  }
  
  /* Header must be at least HEAD_MINSIZE bytes */
  if (status && (head_len < HEAD_MINSIZE)) {
    status = 0;
  }
  
  /* Read format tag and make sure it is uncompressed -- note that if
   * you are checking for compressed tags, you would have to read this
   * as a signed integer rather than as an unsigned integer here */
  if (status) {
    if (readInt16LE(pwv->fp, &status) != 1) {
      status = 0;
    }
  }
  
  /* Read channel count, make sure it is in range, and store in
   * structure */
  if (status) {
    ui16 = readInt16LE(pwv->fp, &status);
    if (status && ((ui16 < 1) || (ui16 > MAX_CHANNELS))) {
      status = 0;
    }
    if (status) {
      pwv->ch_count = (int) ui16;
    }
  }
  
  /* Read sample rate, make sure it is in range, and store in
   * structure */
  if (status) {
    ui32 = readInt32LE(pwv->fp, &status);
    if (status && ((ui32 < MIN_RATE) || (ui32 > MAX_RATE))) {
      status = 0;
    }
    if (status) {
      pwv->samp_rate = (int32_t) ui32;
    }
  }
  
  /* Ignore the average bytes per second and block align fields */
  if (status) {
    ui32 = readInt32LE(pwv->fp, &status);
    ui16 = readInt16LE(pwv->fp, &status);
  }
  
  /* Read bits per sample and make sure it is 16 */
  if (status) {
    if (readInt16LE(pwv->fp, &status) != 16) {
      status = 0;
    }
  }
  
  /* Now, find the data chunk */
  if (status) {
    audio_len = seekChunk(pwv->fp, FOURCC_WAVE, FOURCC_DATA);
    if (audio_len < 0) {
      status = 0;
    }
  }
  
  /* The audio offset is the current position */
  if (status) {
    pwv->audio_offs = (int32_t) ftell(pwv->fp);
    if (pwv->audio_offs == -1) {
      status = 0;
    }
  }
  
  /* Make sure that if the audio data length is greater than zero, it is
   * a multiple of two times the number of channels, so that we only
   * have complete samples in the data */
  if (status && (audio_len > 0)) {
    if ((audio_len % (2 * pwv->ch_count)) != 0) {
      status = 0;
    }
  }
  
  /* Compute the number of samples */
  if (status) {
    pwv->samp_count = audio_len / (2 * pwv->ch_count);
  }
  
  /* If failure, close WAVE structure if open */
  if (!status) {
    closeWAV(pwv);
    pwv = NULL;
  }
  
  /* Return new WAVE structure or NULL */
  return pwv;
}

/*
 * closeWAV function.
 */
void closeWAV(WAVE_FILE *pwv) {
  
  /* Only proceed if non-NULL */
  if (pwv != NULL) {
  
    /* Close the file handle if open */
    if (pwv->fp != NULL) {
      fclose(pwv->fp);
      pwv->fp = NULL;
    }
  
    /* Release the structure */
    free(pwv);
  }
}

/*
 * writeWAVHeader function.
 */
int writeWAVHeader(
    FILE    * fp,
    int       ch_count,
    int32_t   samp_rate,
    int32_t   samp_count) {
  
  int status = 1;
  int32_t total_size = 0;
  int32_t avg_bytes = 0;
  int32_t block_align = 0;
  
  /* Check parameters */
  if (fp == NULL) {
    abort();
  }
  if ((ch_count < 1) || (ch_count > MAX_CHANNELS)) {
    abort();
  }
  if ((samp_rate < MIN_RATE) || (samp_rate > MAX_RATE)) {
    abort();
  }
  if (samp_count < 0) {
    abort();
  }
  
  /* Compute the total size of the output WAV file */
  total_size = castDown(
                (((int64_t) samp_count) * ((int64_t) ch_count) * 2) +
                ((int64_t) FULLHEAD_SIZE));
  if (total_size < 0) {
    /* Overflowed 32-bit range */
    status = 0;
  }
  
  /* Fail if total size is above MAX_RIFF_LEN */
  if (status && (total_size > MAX_RIFF_LEN)) {
    status = 0;
  }
  
  /* Compute average rate and block align */
  if (status) {
    block_align = ch_count * 2;
    avg_bytes = block_align * samp_rate;
  }
  
  /* Write the full header */
  if (status) {
    writeInt32BE(fp, FOURCC_RIFF);
    writeInt32LE(fp, (uint32_t) (total_size - 8));
    writeInt32BE(fp, FOURCC_WAVE);
    
    writeInt32BE(fp, FOURCC_FMT);
    writeInt32LE(fp, 16);
    
    writeInt16LE(fp, 1);
    writeInt16LE(fp, (uint16_t) ch_count);
    writeInt32LE(fp, (uint32_t) samp_rate);
    writeInt32LE(fp, (uint32_t) avg_bytes);
    writeInt16LE(fp, (uint16_t) block_align);
    writeInt16LE(fp, 16);
    
    writeInt32BE(fp, FOURCC_DATA);
    writeInt32LE(fp, (uint32_t) (total_size - FULLHEAD_SIZE));
  }
  
  /* Return status */
  return status;
}

/*
 * seekSample function.
 */
int seekSample(FILE *fp, int32_t audio_offs, int32_t i) {
  
  int status = 1;
  
  /* Check parameters */
  if ((fp == NULL) || (audio_offs < 0) || (i < 0)) {
    abort();
  }
  
  /* Multiply i by two, failing if overflow */
  if (i <= INT32_MAX / 2) {
    i *= 2;
  } else {
    status = 0;
  }
  
  /* Add (2*i) to audio_offs, failing if overflow */
  if (status) {
    if (audio_offs <= INT32_MAX - i) {
      audio_offs += i;
    } else {
      status = 0;
    }
  }
  
  /* Seek to position */
  if (status) {
    if (fseek(fp, (long) audio_offs, SEEK_SET)) {
      status = 0;
    }
  }
  
  /* Return status */
  return status;
}

/*
 * decodeSample function.
 */
int32_t decodeSample(int b1, int b2) {
  
  int32_t v = 0;
  
  /* Mask input bytes */
  b1 = (b1 & 0xff);
  b2 = (b2 & 0xff);
  
  /* Get unsigned value */
  v = (((int32_t) b2) << 8) | ((int32_t) b1);
  
  /* If unsigned value greater than 32767, subtract it by 65536 */
  if (v > 32767) {
    v -= INT32_C(65536);
  }
  
  /* Return decoded value */
  return v;
}
