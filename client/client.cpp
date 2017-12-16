#include <iostream>

#include <cpr/cpr.h>
#include <string>
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

using namespace rapidjson;

extern "C" {
	int* post(int* json){
		Document* d = (Document*)json;
		if((*d).HasMember("url") && (*d).HasMember("payload")) {
			const char* url = ((*d)["url"])["prim_val"].GetString();
			const char* payload = ((*d)["payload"])["prim_val"].GetString();
			std::string urlCpp(url);
			auto r = cpr::Post(cpr::Url{urlCpp}, cpr::Body{payload},cpr::Header{{"Content-Type","application/json"}});
			std::cout << r.text << std::endl;
		}

		return json;
	}

	int* get(int* json){
		Document* d = (Document*)json;
		if((*d).HasMember("url")) {
			const char* url = ((*d)["url"])["prim_val"].GetString();
			const char* body = ((*d)["body"])["prim_val"].GetString();
			std::string urlCpp(url);
			auto r = cpr::Get(cpr::Url{urlCpp}, cpr::Payload{{"arg", body}});
			std::cout << r.text << std::endl;
		}
		return json;
	}
}
