#include <stdio.h>
#include "requests.h"
#include <curl/curl.h>


int* post(const char* url)
{
	/*req_t req;                        
	CURL *curl = requests_init(&req); 
	char *data = "{ \"arg\" : \"Pedro\" }";
	char* headers[] = {"Accept: application/json", "Content-Type: application/json"};
	requests_post_headers(curl, &req, url, data, headers, 2);*/
	CURL *hnd = curl_easy_init();
	curl_easy_setopt(hnd, CURLOPT_CUSTOMREQUEST, "POST");
	curl_easy_setopt(hnd, CURLOPT_URL, "http://35.194.4.65:8000/echo");

	struct curl_slist *headers = NULL;
	headers = curl_slist_append(headers, "content-type: multipart/form-data");
curl_easy_setopt(hnd, CURLOPT_HTTPHEADER, headers);

curl_easy_setopt(hnd, CURLOPT_POSTFIELDS, "------WebKitFormBoundary7MA4YWxkTrZu0gW\r\nContent-Disposition: form-data; name=\"arg\"\r\n\r\ndd\r\n------WebKitFormBoundary7MA4YWxkTrZu0gW--");

CURLcode ret = curl_easy_perform(hnd);
	//printf("Request URL: %s\n", req.url);
	//printf("Response Code: %lu\n", req.code);
	//printf("Response Body:\n%s", req.text);

	//requests_close(&req); /* clean up */
	int val = 9;
	return &val;
}
