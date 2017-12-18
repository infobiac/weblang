#include <iostream>

#include <cpr/cpr.h>
#include <string>
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"
#include "../jsonlib/jsonlib.h"

using namespace rapidjson;

extern "C" {
	int* post(const char* url, int* json, const char* key, const char* secret){

		try{
			const char* payload = tostring(json);
			std::string urlCpp(url);
			auto r = cpr::Post(cpr::Url{urlCpp}, cpr::Body{payload},cpr::Header{{"Content-Type","application/json"}});
			char* ret = (char*) malloc(strlen(r.text.c_str())+1);
			strcpy(ret, r.text.c_str());
			return (int *) ret;
		}
		catch(...){
			std::cout << "Post failed" << std::endl;
			throw "Post failed";
		}
	}

	int* get(const char* url, int* json, const char* key, const char* secret) {
		try{
			const char* body = tostring(json);
			std::string urlCpp(url);
			auto r = cpr::Get(cpr::Url{urlCpp}, cpr::Payload{{"arg", body}});
			char* ret = (char*) malloc(strlen(r.text.c_str())+1);
			strcpy(ret, r.text.c_str());
			return (int *) ret;
		}
		catch(...){
			std::cout << "Get failed" << std::endl;
			throw "Please provide a body";
		}
	}
}
