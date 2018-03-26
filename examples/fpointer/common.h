#pragma once

#define __attr_zerostack __attribute__((annotate("SENSITIVE")))
#define __attr_hash_init  __attribute__((type_annotate("hash_init")))
#define __attr_hash_init2  __attribute__((type_annotate("hash_init2")))

typedef __attr_hash_init void (*hash_init)(char *, size_t);

extern __attr_hash_init void sha256_init(char *b, size_t len);
extern __attr_hash_init void sha512_init(char *b, size_t len);
