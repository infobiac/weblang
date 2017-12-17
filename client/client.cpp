#include <iostream>

#include <cpr/cpr.h>
#include <string>
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

using namespace rapidjson;

extern "C" {
	int* post(const char* url, int* json){
		Document* d = (Document*)json;
		if((*d).HasMember("payload")) {
			const char* payload = ((*d)["payload"])["prim_val"].GetString();
			std::string urlCpp(url);
			auto r = cpr::Post(cpr::Url{urlCpp}, cpr::Body{payload},cpr::Header{{"Content-Type","application/json"}});
			char* ret = (char*) malloc(strlen(r.text.c_str())+1);
			strcpy(ret, r.text.c_str());
			return (int *) ret;
		}
		throw "Please provide a payload!";
	}

	int* get(const char* url, int* json){
		Document* d = (Document*)json;
		if((*d).HasMember("body")) {
			const char* body = ((*d)["body"])["prim_val"].GetString();
			std::string urlCpp(url);
			auto r = cpr::Get(cpr::Url{urlCpp}, cpr::Payload{{"arg", body}});
			char* ret = (char*) malloc(strlen(r.text.c_str())+1);
			strcpy(ret, r.text.c_str());
			return (int *) ret;
		}
		throw "Please provide a body";
	}
}
