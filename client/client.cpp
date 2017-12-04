#include <iostream>

#include <cpr/cpr.h>
#include <string>
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"
using namespace rapidjson;

extern "C" {
	int* post(const char* jsonString){
		Document d;
		d.Parse(jsonString);
		if(d.HasMember("url")) {
			const char* url = (d["url"]).GetString();
			const char* payload = (d["payload"]).GetString();
			std::string urlCpp(url);
			auto r = cpr::Post(cpr::Url{urlCpp}, cpr::Body{payload},cpr::Header{{"Content-Type","application/json"}});
			std::cout << r.text << std::endl;
		}
		int a = 0;
		return &a;
	}

	int* get(const char* jsonString){
		Document d;
		d.Parse(jsonString);
		if(d.HasMember("url")) {
			const char* url = (d["url"]).GetString();
			const char* body = (d["body"]).GetString();
			std::string urlCpp(url);
			auto r = cpr::Get(cpr::Url{urlCpp}, cpr::Payload{{"arg", body}});
			std::cout << r.text << std::endl;
		}
		int a = 0;
		return &a;
	}
}
