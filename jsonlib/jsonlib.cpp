#include "rapidjson/include/rapidjson/document.h"
#include "rapidjson/include/rapidjson/writer.h"
#include "rapidjson/include/rapidjson/stringbuffer.h"
#include <iostream>
using namespace rapidjson;

class json {
public:
	json(const char* s){
		d.Parse(s);
	}

	json(){
		d.Parse("");
	}

	Value& getp(const char* key){
		return d[key];
	}

	std::string gets(const char* key){
		return getp(key).GetString();
	}

	
		
private:
	Document d;
};

int main(){
	const char* test = "{\"test\":\"christophe\"}";
	json j(test);
	std::cout << j.gets("test") << std::endl;
	return 0;
}

