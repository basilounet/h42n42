FROM	node:24.20.0-alpine3.24


RUN		[	"mkdir", "-p", "/home/node/app" 		]
ADD		[	"conf/index.html",	"/home/node/app"	]


WORKDIR 	/home/node/app
EXPOSE 		8080
RUN		[	"npm", "install", "-g", "live-server"								]
CMD     [	"live-server", "--port=8080", "--host=0.0.0.0", "--no-browser", "."	]