/*
 * serve(1) is a simple HTTP server that is intended to be used for 
 * static website development. Check `serve -h` for usage.
 *
 * TODO: Add Content-Type header to the response
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>

int sockfd;

void interrupt(int signal)
{
	close(sockfd);
	printf("\n");
	exit(0);
}

#define CHARSET "utf-8"
#define CHUNK_SIZE 4096 /* filesize(2) is not portable, unfortunately */
#define REQLINE_LEN 256  /* maximum length of HTTP Request-Line */
#define DEFAULT_PORT 36895 /* htons(8080) */

#define die(msg) { perror(msg); exit(1); } 

#define STATUS "HTTP/1.1 %s\n"
#define NOT_FOUND "404 Not Found"
#define OK "200 OK"

int main(int argc, char **argv)
{
	uint16_t port = DEFAULT_PORT;
	int err, opt;

	while ((opt = getopt(argc, argv, "h:p:")) != -1) {
		switch (opt) {
		case 'p':
			port = htons(atoi(optarg));
			break;
		default:
			puts("Usage: serve [-h 127.0.0.1] [-p 8080] [directory]");
			exit(1);
             	}

	}

	sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd < 0) die("failed to create a socket");
	signal(SIGINT, interrupt);

	struct sockaddr_in server;

	server.sin_family = AF_INET;
	server.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	server.sin_port = port;

	err = bind(sockfd, (struct sockaddr *) &server, sizeof(server));
	if (err < 0) die("failed to bind server socket");

	err = listen(sockfd, 5);
	if (err < 0) die("failed to start listening server socket");

	char reqbuf[REQLINE_LEN], fbuf[CHUNK_SIZE];
	int connfd, i, j, k;
	FILE *connfp;

	for (;;) {
		bzero(reqbuf, REQLINE_LEN);

		connfd = accept(sockfd, NULL, NULL);

		if (connfd < 0) {
			perror("failed to accept incoming connection");
			goto close;
		}

		err = read(connfd, reqbuf, sizeof(reqbuf));

		if (err < 0) {
			perror("failed to read from client socket");
			goto close; 
		}

		connfp = fdopen(connfd, "w");

		if (connfp == NULL) {
			perror("failed to create a file pointer for client socket");
			goto close;
		}

		if (reqbuf[0] == 'G' && reqbuf[1] == 'E' && reqbuf[2] == 'T') {
			for (i = j = strlen("GET /"); reqbuf[i+1] != ' ' && reqbuf[i] != '\0'; i++);

			char *fname = malloc((i - j) * sizeof(char)); /* TODO: check for err */
			for(k = 0; i >= (j+k); k++) fname[k] = reqbuf[j+k];
			fname[k] = '\0';

			if (access(fname, R_OK) != -1) {
				bzero(fbuf, CHUNK_SIZE);

				FILE *fp = fopen(fname, "r");

				if (fp == NULL) {
					perror("failed to open requested file");
					goto free;
				}

				err = fprintf(connfp, STATUS, OK);

				if (err < 0) {
					perror("failed to send HTTP 200 OK Response Header");
					goto free;
				}

				err = fprintf(connfp, "Content-Type: %s; charset=%s\n\n", "text/html", CHARSET);

				if (err < 0) {
					perror("failed to send Content-Type header");
					goto free;
				}

				//for(i = j = ftell(fp); i == 0;

				err = fread(fbuf, sizeof(char), CHUNK_SIZE, fp);

				if (err < 0) {
					perror("failed to read chunk from file");
					goto free;
				}

				err = fputs(fbuf, connfp);

				if (err < 0) {
					perror("failed to write file chunk to socket");
					goto free;
				}
			} else {
				err = fprintf(connfp, STATUS, NOT_FOUND);

				if (err < 0) {
					perror("failed to send HTTP 404 Not Found Response Header");
					goto free;
				}
			}

free:
			free(fname);
		}

close:
		fclose(connfp);
	}
}
