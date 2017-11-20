#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"
#include <iostream>
using namespace rapidjson;
extern "C"{

int* json(const char*s){
	std::cout << "Storing: " << s << std::endl;
	Document* d = new Document();
	(*d).Parse(s);
	return (int*)d;
}
/*
int* json(){
	Document* d = new Document();
	(*d).Parse("");
	return (int*)d;
}*/

int test(const char* s){
	std::cout << "HI" << std::endl;
	return 3;
}

Value& getp(int* intdoc, const char* key){
	Document* d = (Document*)intdoc;
	return (*d)[key];
}

const char* jgets(int* intdoc, const char* key){
	Document* d = (Document*) intdoc;
	if((*d).HasMember(key)){
		std::cout << "we got it" << std::endl;
		return getp((int*)d, key).GetString();
	}
	else{
		std::cout << "not here" << std::endl;
		return "";
	}
}

std::string adds(int *intdoc, const char* key, const char* value){
	Document* d = (Document*)intdoc;
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

std::string tostring(int *tempdoc){
	Document* d = (Document *)tempdoc;
	StringBuffer buff;
	buff.Clear();
	Writer<StringBuffer> writer(buff);
	(*d).Accept(writer);
	return std::string(buff.GetString());
}
/*
 // Test function
int main(){
	const char* test = "{\"test\":\"christophe\"}";
	std::cout << sizeof(int) <<std::endl;
	int* j = json(test);
	adds(j, "boop", "is");
	adds(j, "test", "w");
	std::cout << tostring(j) << std::endl;
	std::cout << (*((Document*)j))["test"].GetString() << std::endl;
	return 0;
}
*/
}
