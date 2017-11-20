#include "rapidjson/include/rapidjson/document.h"
#include "rapidjson/include/rapidjson/writer.h"
#include "rapidjson/include/rapidjson/stringbuffer.h"
#include <iostream>
using namespace rapidjson;

Document* json(const char*s){
	Document* d = new Document();
	(*d).Parse(s);
	return d;
}

Document* json(){
	Document* d = new Document();
	(*d).Parse("");
	return d;
}

Value& getp(Document* d, const char* key){
	return (*d)[key];
}

std::string gets(Document *d, const char* key){
	if((*d).HasMember(key)){
		return getp(d, key).GetString();
	}
	else{
		return "";
	}
}

std::string adds(Document *d, const char* key, const char* value){
	if ((*d).HasMember(key)){
		(*d)[key].SetString(value, strlen(value), (*d).GetAllocator());
		return key;
	}
	else{
		Value tempkey;
		Value tempvalue;
		tempkey.SetString(key, (*d).GetAllocator());
		tempvalue.SetString(value, (*d).GetAllocator());
		(*d).AddMember(tempkey.Move(), tempvalue.Move(), (*d).GetAllocator());
		return key;
	}
}

std::string tostring(Document *d){
	StringBuffer buff;
	buff.Clear();
	Writer<StringBuffer> writer(buff);
	(*d).Accept(writer);
	return std::string(buff.GetString());
}

int main(){
	const char* test = "{\"test\":\"christophe\"}";

	Document* j = json(test);
	adds(j, "boop", "is");
	adds(j, "test", "w");
	std::cout << tostring(j) << std::endl;
	std::cout << (*j)["test"].GetString() << std::endl;
	return 0;
}

