#ifndef WAV_H_INCLUDED
#define WAV_H_INCLUDED

/*
 * wav.h
 * =====
 * 
 * Kaltag WAV file codec.
 */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/*
 * The maximum number of channels in an input WAVE file.
 */
#define MAX_CHANNELS (64)

/*
 * The minimum and maximum allowable sampling rates in samples per
 * second.
 */
#define MIN_RATE INT32_C(1024)
#define MAX_RATE INT32_C(192000)

/*
 * Structure holding a read-only WAVE file handle and information about
 * the file.
 * 
 * Supported WAVE files are always uncompressed, linear PCM with 16-bit
 * signed samples in little-endian order.
 */
typedef struct {
  
  /*
   * Read-only handle to the WAVE file.
   * 
   * This should support random access and be open for reading.
   */
  FILE *fp;
  
  /*
   * The number of channels in the data.
   * 
   * Range is [1, MAX_CHANNELS].
   */
  int ch_count;
  
  /*
   * The sampling rate in samples per second.
   * 
   * Range is [MIN_RATE, MAX_RATE].
   */
  int32_t samp_rate;
  
  /*
   * The file offset of the start of the audio data.
   * 
   * Positioned immediately after the data chunk if samp_count is zero.
   */
  int32_t audio_offs;
  
  /*
   * The length in complete samples of the audio data.
   * 
   * This may be zero.
   */
  int32_t samp_count;
  
} WAVE_FILE;

/*
 * Perform a safe down-cast from 64-bit to 32-bit.
 * 
 * v is the 64-bit value down-cast.  It must not be negative or a fault
 * occurs.
 * 
 * The return value is v as a 32-bit integer, unless v is out of range,
 * in which case -1 is returned.
 * 
 * Parameters:
 * 
 *   v - the value to down-cast
 * 
 * Return:
 * 
 *   the down-cast value, or -1
 */
int32_t castDown(int64_t v);

/*
 * Load the WAVE file at the given path into a WAVE_FILE structure.
 * 
 * The input WAV file must be uncompressed (linear PCM) audio, using
 * 16-bit signed samples in little-endian (LE) order.  The channel count
 * must be in range [1, MAX_CHANNELS], and the sample rate must be in
 * range [MIN_RATE, MAX_RATE].  The WAV file must have a length that
 * does not exceed one gigabyte.
 * 
 * The structure should eventually be freed with closeWAV().
 * 
 * If the WAVE file can't be loaded successfully, NULL is returned.
 * 
 * If successful, the file pointer is positioned at the start of the
 * WAV data (if there is any).
 * 
 * Parameters:
 * 
 *   pPath - the path to the WAVE file to load
 * 
 * Return:
 * 
 *   a new WAVE file structure, or NULL
 */
WAVE_FILE *loadWAV(const char *pPath);

/*
 * Close a WAVE_FILE structure and release it.
 * 
 * If NULL is passed, nothing is done.
 * 
 * This also closes the file handle.
 * 
 * Parameters:
 * 
 *   pwv - the WAVE file to close, or NULL
 */
void closeWAV(WAVE_FILE *pwv);

/*
 * Write a complete WAV file header to the given output file.
 * 
 * fp is the file to write the header to.  It must be open for writing
 * or undefined behavior occurs.  Writing is fully sequential.  At the
 * end of a successful function call, the file pointer will be
 * positioned where the audio samples need to be written.
 * 
 * ch_count is the number of channels.  It must be in [1, MAX_CHANNELS].
 * 
 * samp_rate is the sampling rate in samples per second.  It must be in
 * range [MIN_RATE, MAX_RATE].
 * 
 * samp_count is the number of samples to write to the file.  It must be
 * zero or greater.  If the sample count causes the file to be greater
 * than an internal maximum limit, the function fails.
 * 
 * Parameters:
 * 
 *   fp - the file
 * 
 *   ch_count - the channel count
 * 
 *   samp_rate - the sampling rate
 * 
 *   samp_count - the number of samples that will be written
 * 
 * Return:
 * 
 *   non-zero if successful, zero if error
 */
int writeWAVHeader(
    FILE    * fp,
    int       ch_count,
    int32_t   samp_rate,
    int32_t   samp_count);

/*
 * Position a given file at the given sample channel value offset.
 * 
 * fp is the file to position.  It must be open and support random
 * access or undefined behavior occurs.
 * 
 * audio_offs is the byte offset within the file at which the sample
 * data starts.  It must be zero or greater.
 * 
 * i is the 16-bit sample channel offset value.  It must be zero or
 * greater.  This refers to individual channel samples, not full
 * samples!
 * 
 * The function fails if the desired location would be out of int32_t
 * range or if there is an I/O error while seeking.  This function does
 * not check whether the desired location is actually within the file.
 * 
 * Parameters:
 * 
 *   fp - the file to position
 * 
 *   audio_offs - the byte offset of the sample data
 * 
 *   i - the channel sample offset
 * 
 * Return:
 * 
 *   non-zero if successful, zero if failure
 */
int seekSample(FILE *fp, int32_t audio_offs, int32_t i);

/*
 * Given two byte values representing a 16-bit signed two's complement
 * little-endian sample, return the decoded numeric value.
 * 
 * b1 is the first encoded byte and b2 is the second encoded byte.  Both
 * should be in unsigned range 0-255.  Bitwise AND is applied to mask
 * off all but the lowest eight bits.
 * 
 * The return value is in range [-32768, 32767].
 * 
 * Parameters:
 * 
 *   b1 - the first byte
 * 
 *   b2 - the second byte
 * 
 * Return:
 * 
 *   the decoded numeric value
 */
int32_t decodeSample(int b1, int b2);

#endif
