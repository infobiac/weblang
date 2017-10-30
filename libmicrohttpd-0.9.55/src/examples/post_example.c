/*
     This file is part of libmicrohttpd
     (C) 2011 Christian Grothoff (and other contributing authors)

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 2.1 of the License, or (at your option) any later version.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the GNU Lesser General Public
     License along with this library; if not, write to the Free Software
     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */
/**
 * @file post_example.c
 * @brief example for processing POST requests using libmicrohttpd
 * @author Christian Grothoff
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <microhttpd.h>
#include <stdlib.h>


/**
 * Invalid method page.
 */
#define METHOD_ERROR "<html><head><title>Illegal request</title></head><body>Go away.</body></html>"

/**
 * Not M method page.
 */
#define NOT_FOUND_ERROR "<html><head><title>Illegal request</title></head><body>Go away.</body></html>"

/**
 * Name of our cookie.
 */
#define COOKIE_NAME "session"


/**
 * State we keep for each user/session/browser.
 */
struct Session
{
        /**
         * We keep all sessions in a linked list.
         */
        struct Session *next;

        /**
         * Unique ID for this session.
         */
        char sid[33];

        /**
         * Reference counter giving the number of connections
         * currently using this session.
         */
        unsigned int rc;

        /**
         * Time when this session was last active.
         */
        time_t start;

        /**
         * String submitted via form.
         */
        char value_1[64];

        /**
         * Another value submitted via form.
         */
        char value_2[64];

};


/**
 * Data kept per request.
 */
struct Request
{

        /**
         * Associated session.
         */
        struct Session *session;

        /**
         * Post processor handling form data (IF this is
         * a POST request).
         */
        struct MHD_PostProcessor *pp;

        /**
         * URL to serve in response to this POST (if this request
         * was a 'POST')
         */
        const char *post_url;

};

/**
 * Linked list of all active sessions.  Yes, O(n) but a
 * hash table would be overkill for a simple example...
 */
static struct Session *sessions;

/**
 * Return the session handle for this connection, or
 * create one if this is a new user.
 */
static struct Session *
get_session (struct MHD_Connection *connection)
{
        struct Session *ret;
        const char *cookie;

        cookie = MHD_lookup_connection_value (connection,
                                              MHD_COOKIE_KIND,
                                              COOKIE_NAME);
        if (cookie != NULL)
        {
                /* find existing session */
                ret = sessions;
                while (NULL != ret)
                {
                        if (0 == strcmp (cookie, ret->sid))
                                break;
                        ret = ret->next;
                }
                if (NULL != ret)
                {
                        ret->rc++;
                        return ret;
                }
        }
        /* create fresh session */
        ret = calloc (1, sizeof (struct Session));
        if (NULL == ret)
        {
                fprintf (stderr, "calloc error: %s\n", strerror (errno));
                return NULL;
        }
        /* not a super-secure way to generate a random session ID,
           but should do for a simple example... */
        snprintf (ret->sid,
                  sizeof (ret->sid),
                  "%X%X%X%X",
                  (unsigned int) random (),
                  (unsigned int) random (),
                  (unsigned int) random (),
                  (unsigned int) random ());
        ret->rc++;
        ret->start = time (NULL);
        ret->next = sessions;
        sessions = ret;
        return ret;
}


/**
 * Add header to response to set a session cookie.
 *
 * @param session session to use
 * @param response response to modify
 */
static void
add_session_cookie (struct Session *session,
                    struct MHD_Response *response)
{
        char cstr[256];
        snprintf (cstr,
                  sizeof (cstr),
                  "%s=%s",
                  COOKIE_NAME,
                  session->sid);
        if (MHD_NO ==
            MHD_add_response_header (response,
                                     MHD_HTTP_HEADER_SET_COOKIE,
                                     cstr))
        {
                fprintf (stderr,
                         "Failed to set session cookie header!\n");
        }
}

/**
 * Handler that adds the 'v1' value to the given HTML code.
 *
 * @param cls a 'const char *' with the HTML webpage to return
 * @param mime mime type to use
 * @param session session handle
 * @param connection connection to use
 */
static int
create_json (const void *cls,
              const char *mime,
              struct Session *session,
              struct MHD_Connection *connection){
        int ret;
        const char *form = cls;
        char *reply;
        struct MHD_Response *response;

        reply = malloc (strlen (form) + strlen (session->value_1) + 1);
        snprintf (reply,
                  strlen (form) + strlen (session->value_1) + 1,
                  form,
                  session->value_1);
        /* return static form */
        response = MHD_create_response_from_buffer (strlen (reply),
                                                    (void *) reply,
                                                    MHD_RESPMEM_MUST_FREE);
        add_session_cookie (session, response);
        MHD_add_response_header (response,
                                 MHD_HTTP_HEADER_CONTENT_ENCODING,
                                 mime);
        ret = MHD_queue_response (connection,
                                  MHD_HTTP_OK,
                                  response);
        MHD_destroy_response (response);
        printf("%s\n", "handler");
        return ret;
}


/**
 * Handler that adds the 'v1' and 'v2' values to the given HTML code.
 *
 * @param cls a 'const char *' with the HTML webpage to return
 * @param mime mime type to use
 * @param session session handle
 * @param connection connection to use
 */
static int
fill_v1_v2_form (const void *cls,
                 const char *mime,
                 struct Session *session,
                 struct MHD_Connection *connection)
{
        int ret;
        const char *form = cls;
        char *reply;
        struct MHD_Response *response;

        reply = malloc (strlen (form) + strlen (session->value_1) + strlen (session->value_2) + 1);
        snprintf (reply,
                  strlen (form) + strlen (session->value_1) + strlen (session->value_2) + 1,
                  form,
                  session->value_1);
        /* return static form */
        response = MHD_create_response_from_buffer (strlen (reply),
                                                    (void *) reply,
                                                    MHD_RESPMEM_MUST_FREE);
        add_session_cookie (session, response);
        MHD_add_response_header (response,
                                 MHD_HTTP_HEADER_CONTENT_ENCODING,
                                 mime);
        ret = MHD_queue_response (connection,
                                  MHD_HTTP_OK,
                                  response);
        MHD_destroy_response (response);
        return ret;
}


/**
 * Handler used to generate a 404 reply.
 *
 * @param cls a 'const char *' with the HTML webpage to return
 * @param mime mime type to use
 * @param session session handle
 * @param connection connection to use
 */
static int
not_found_page (const void *cls,
                const char *mime,
                struct Session *session,
                struct MHD_Connection *connection)
{
        int ret;
        struct MHD_Response *response;

        /* unsupported HTTP method */
        response = MHD_create_response_from_buffer (strlen (NOT_FOUND_ERROR),
                                                    (void *) NOT_FOUND_ERROR,
                                                    MHD_RESPMEM_PERSISTENT);
        ret = MHD_queue_response (connection,
                                  MHD_HTTP_NOT_FOUND,
                                  response);
        MHD_add_response_header (response,
                                 MHD_HTTP_HEADER_CONTENT_ENCODING,
                                 mime);
        MHD_destroy_response (response);
        return ret;
}

/**
 * Iterator over key-value pairs where the value
 * maybe made available in increments and/or may
 * not be zero-terminated.  Used for processing
 * POST data.
 *
 * @param cls user-specified closure
 * @param kind type of the value
 * @param key 0-terminated key for the value
 * @param filename name of the uploaded file, NULL if not known
 * @param content_type mime-type of the data, NULL if not known
 * @param transfer_encoding encoding of the data, NULL if not known
 * @param data pointer to size bytes of data at the
 *              specified offset
 * @param off offset of data in the overall value
 * @param size number of bytes in data available
 * @return MHD_YES to continue iterating,
 *         MHD_NO to abort the iteration
 */
static int
post_iterator (void *cls,
               enum MHD_ValueKind kind,
               const char *key,
               const char *filename,
               const char *content_type,
               const char *transfer_encoding,
               const char *data, uint64_t off, size_t size)
{
        struct Request *request = cls;
        struct Session *session = request->session;

        if (0 == strcmp ("arg", key)) {
            if (size + off > sizeof(session->value_1))
                    size = sizeof (session->value_1) - off;
            memcpy (&session->value_1[off],
                    data,
                    size);
            if (size + off < sizeof (session->value_1))
                    session->value_1[size+off] = '\0';
            printf("body: %s\n", data);
            return MHD_YES;
        }
        fprintf (stderr, "Unsupported form value `%s'\n", key);
        return MHD_YES;
}


/**
 * Main MHD callback for handling requests.
 *
 *
 * @param cls argument given together with the function
 *        pointer when the handler was registered with MHD
 * @param url the requested url
 * @param method the HTTP method used ("GET", "PUT", etc.)
 * @param version the HTTP version string (i.e. "HTTP/1.1")
 * @param upload_data the data being uploaded (excluding HEADERS,
 *        for a POST that fits into memory and that is encoded
 *        with a supported encoding, the POST data will NOT be
 *        given in upload_data and is instead available as
 *        part of MHD_get_connection_values; very large POST
 *        data *will* be made available incrementally in
 *        upload_data)
 * @param upload_data_size set initially to the size of the
 *        upload_data provided; the method must update this
 *        value to the number of bytes NOT processed;
 * @param con_cls pointer that the callback can set to some
 *        address and that will be preserved by MHD for future
 *        calls for this request; since the access handler may
 *        be called many times (i.e., for a PUT/POST operation
 *        with plenty of upload data) this allows the application
 *        to easily associate some request-specific state.
 *        If necessary, this state can be cleaned up in the
 *        global "MHD_RequestCompleted" callback (which
 *        can be set with the MHD_OPTION_NOTIFY_COMPLETED).
 *        Initially, <tt>*con_cls</tt> will be NULL.
 * @return MHS_YES if the connection was handled successfully,
 *         MHS_NO if the socket must be closed due to a serios
 *         error while handling the request
 */
static int
create_response (void *cls,
                 struct MHD_Connection *connection,
                 const char *url,
                 const char *method,
                 const char *version,
                 const char *upload_data,
                 size_t *upload_data_size,
                 void **ptr)
{
        struct MHD_Response *response;
        struct Request *request;
        struct Session *session;
        int ret;
        unsigned int i;

        request = *ptr;
        if (NULL == request)
        {
                request = calloc (1, sizeof (struct Request));
                if (NULL == request)
                {
                        fprintf (stderr, "calloc error: %s\n", strerror (errno));
                        return MHD_NO;
                }
                *ptr = request;
                if (0 == strcmp (method, MHD_HTTP_METHOD_POST))
                {
                        request->pp = MHD_create_post_processor (connection, 1024,
                                                                 &post_iterator, request);
                        if (NULL == request->pp)
                        {
                                fprintf (stderr, "Failed to setup post processor for `%s'\n",
                                         url);
                                return MHD_NO; /* internal error */
                        }
                }
                return MHD_YES;
        }
        if (NULL == request->session)
        {
                request->session = get_session (connection);
                if (NULL == request->session)
                {
                        fprintf (stderr, "Failed to setup session for `%s'\n",
                                 url);
                        return MHD_NO; /* internal error */
                }
        }
        session = request->session;
        session->start = time (NULL);
        if (0 == strcmp (method, MHD_HTTP_METHOD_POST))
        {
                printf("%s at %s\n", "GOT POST", url);
                /* evaluate POST data */
                MHD_post_process (request->pp,
                                  upload_data,
                                  *upload_data_size);
                if (0 != *upload_data_size)
                {
                    printf("%s\n", "0");
                        *upload_data_size = 0;
                        return MHD_YES;
                }
                /* done with POST data, serve response */
                MHD_destroy_post_processor (request->pp);
                request->pp = NULL;
                method = MHD_HTTP_METHOD_GET; /* fake 'GET' */
                if (NULL != request->post_url) {
                        url = request->post_url;
                }
                
                /* Run weblang command */

		char *buffer;
		if(strcmp(url, "/helloworld") == 0) {
			char* HW = "stack exec --nix weblang < /home/jordanvega/plt/examples/parsing-hello-world-example.wl > test.txt";
			FILE *file = popen(HW, "r");
			fclose(file);
			
			FILE *fp;
			long lSize;

			fp = fopen ( "test.txt" , "rb" );
			if( !fp ) perror("cant read"),exit(1);

			fseek( fp , 0L , SEEK_END);
			lSize = ftell( fp );
			rewind( fp );

			/* allocate memory for entire content */
			buffer = calloc( 1, lSize+1 );
			if( !buffer ) fclose(fp),fputs("memory alloc fails",stderr),exit(1);

			/* copy the file into the buffer */
			if( 1!=fread( buffer , lSize, 1 , fp) )
				  fclose(fp),free(buffer),fputs("entire read fails",stderr),exit(1);
			fclose(fp);
			ret = create_json(buffer, "application/json", session, connection);
			system("rm test.txt");
			free(buffer);


		}	
		else
			ret = create_json("No function found in weblang that matches endpoint", "application/json", session, connection);
		/* do your work here, buffer is a string contains the whole text */
				
                if (ret != MHD_YES)
                        fprintf (stderr, "Failed to create page for `%s'\n",
                                 url);
                return ret;
        }
        /* unsupported HTTP method */
        response = MHD_create_response_from_buffer (strlen (METHOD_ERROR),
                                                    (void *) METHOD_ERROR,
                                                    MHD_RESPMEM_PERSISTENT);
        ret = MHD_queue_response (connection,
                                  MHD_HTTP_METHOD_NOT_ACCEPTABLE,
                                  response);
        MHD_destroy_response (response);
        return ret;
}


/**
 * Callback called upon completion of a request.
 * Decrements session reference counter.
 *
 * @param cls not used
 * @param connection connection that completed
 * @param con_cls session handle
 * @param toe status code
 */
static void
request_completed_callback (void *cls,
                            struct MHD_Connection *connection,
                            void **con_cls,
                            enum MHD_RequestTerminationCode toe)
{
        struct Request *request = *con_cls;

        if (NULL == request)
                return;
        if (NULL != request->session)
                request->session->rc--;
        if (NULL != request->pp)
                MHD_destroy_post_processor (request->pp);
        free (request);
}


/**
 * Clean up handles of sessions that have been idle for
 * too long.
 */
static void
expire_sessions ()
{
        struct Session *pos;
        struct Session *prev;
        struct Session *next;
        time_t now;

        now = time (NULL);
        prev = NULL;
        pos = sessions;
        while (NULL != pos)
        {
                next = pos->next;
                if (now - pos->start > 60 * 60)
                {
                        /* expire sessions after 1h */
                        if (NULL == prev)
                                sessions = pos->next;
                        else
                                prev->next = next;
                        free (pos);
                }
                else
                        prev = pos;
                pos = next;
        }
}


/**
 * Call with the port number as the only argument.
 * Never terminates (other than by signals, such as CTRL-C).
 */
int
main (int argc, char *const *argv)
{
        struct MHD_Daemon *d;
        struct timeval tv;
        struct timeval *tvp;
        fd_set rs;
        fd_set ws;
        fd_set es;
        int max;
        unsigned MHD_LONG_LONG mhd_timeout;

        if (argc != 2)
        {
                printf ("%s PORT\n", argv[0]);
                return 1;
        }
        /* initialize PRNG */
        srandom ((unsigned int) time (NULL));
        d = MHD_start_daemon (MHD_USE_DEBUG,
                              atoi (argv[1]),
                              NULL, NULL,
                              &create_response, NULL,
                              MHD_OPTION_CONNECTION_TIMEOUT, (unsigned int) 15,
                              MHD_OPTION_NOTIFY_COMPLETED, &request_completed_callback, NULL,
                              MHD_OPTION_END);
        if (NULL == d)
                return 1;
        while (1)
        {
                expire_sessions ();
                max = 0;
                FD_ZERO (&rs);
                FD_ZERO (&ws);
                FD_ZERO (&es);
                if (MHD_YES != MHD_get_fdset (d, &rs, &ws, &es, &max))
                        break; /* fatal internal error */
                if (MHD_get_timeout (d, &mhd_timeout) == MHD_YES)
                {
                        tv.tv_sec = mhd_timeout / 1000;
                        tv.tv_usec = (mhd_timeout - (tv.tv_sec * 1000)) * 1000;
                        tvp = &tv;
                }
                else
                        tvp = NULL;
                select (max + 1, &rs, &ws, &es, tvp);
                MHD_run (d);
        }
        MHD_stop_daemon (d);
        return 0;
}
