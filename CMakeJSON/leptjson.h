﻿#ifndef LEPTJSON_H__
#define LEPTJSON_H__
#pragma once
#include <iostream>

//初始化类型
#define lept_init(v) do { (v)->type = LEPT_NULL; } while(0)


typedef enum{LEPT_NULL, LEPT_FALSE, LEPT_TRUE, LEPT_NUMBER, LEPT_STRING, LEPT_ARRAY, LEPT_OBJECT}	lept_type;

typedef struct {
	union {
		struct { char* s; size_t len; }s;
			double n;
		} u;
	lept_type type;	//一种变体类型，通过type决定结构中哪个成员是有效的
}lept_value;


enum {
    LEPT_PARSE_OK = 0,
    LEPT_PARSE_EXPECT_VALUE,
    LEPT_PARSE_INVALID_VALUE,
    LEPT_PARSE_ROOT_NOT_SINGULAR,
    LEPT_PARSE_NUMBER_TOO_BIG,
    LEPT_PARSE_MISS_QUOTATION_MARK,
    LEPT_PARSE_INVALID_STRING_ESCAPE,
    LEPT_PARSE_INVALID_STRING_CHAR
};


//解析json，传入的JSON文本是一个C字符串
int lept_parse(lept_value* v, const char* json);

//获取值的类型
lept_type lept_get_type(const lept_value* v);

double lept_get_number(const lept_value* v);

void lept_free(lept_value* v);

void lept_set_string(lept_value* v, const char* s, size_t len);

#define lept_set_null(v) lept_free(v)

int lept_get_boolean(const lept_value* v);
void lept_set_boolean(lept_value* v, int b);

double lept_get_number(const lept_value* v);
void lept_set_number(lept_value* v, double n);

const char* lept_get_string(const lept_value* v);
size_t lept_get_string_length(const lept_value* v);
void lept_set_string(lept_value* v, const char* s, size_t len);

#endif