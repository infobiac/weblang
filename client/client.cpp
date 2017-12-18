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
			auto r = cpr::Post(cpr::Url{urlCpp}, cpr::Body{payload},cpr::Header{{"Content-Type","application/json"}}, cpr::Authentication{"73e6c7dd-776b-4ec2-9b4b-965e1a1dcebe","sCatHtlfB0yih7l/lhGpAA=="});
			char* ret = (char*) malloc(strlen(r.text.c_str())+1);
			strcpy(ret, r.text.c_str());
			return (int *) ret;
		}
		catch(...){
			throw std::runtime_error("Failed post");
		}
	}

	int* exposed_post(int* req){
		Document* d = (Document*) req;
		if ((*d).HasMember("url")){
			const char* url = tostring((int*)(&((*d)["url"])));
			int* body;
		       	if((*d).HasMember("body"))
				body = (int*) (&((*d)["body"]));
			else if((*d).HasMember("payload"))
				body = (int*) (&((*d)["payload"]));
			else
				body = (int*) new Document();
			return post(url, body);
		}
		throw std::runtime_error("Post did not contain URL!");
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
			throw std::runtime_error("Failed get");
		}
	}
}
