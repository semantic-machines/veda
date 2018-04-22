package main

import (
	"io"
	"log"
	"unicode/utf8"

	"strings"

	"os"

	"github.com/valyala/fasthttp"
)

//uploadFile handles file uploading from request context
func uploadFile(ctx *fasthttp.RequestCtx) {
	//Get form from context
	form, err := ctx.Request.MultipartForm()
	if err != nil {
		log.Println("@ERR REDING FORM: UPLOAD FILE: ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	//Getting path parts from form
	pathParts := strings.Split(form.Value["path"][0], "/")
	attachmentsPathCurr := attachmentsPath
	//Creating directories for file
	for i := 1; i < len(pathParts); i++ {
		attachmentsPathCurr += "/" + pathParts[i]
		os.Mkdir(attachmentsPathCurr, os.ModePerm)
	}

	//Create file in destination directory
	destFile, err := os.OpenFile(attachmentsPathCurr+"/"+form.Value["uri"][0], os.O_WRONLY|os.O_CREATE, 0666)
	if err != nil {
		log.Println("@ERR CREATING DESTIONTION FILE ON UPLOAD: ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	defer destFile.Close()
	//Open frime form
	srcFile, err := form.File["file"][0].Open()
	if err != nil {
		log.Println("@ERR OPENING FORM FILE ON UPLOAD: ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}
	defer srcFile.Close()

	//Copy srce file from form to destination
	_, err = io.Copy(destFile, srcFile)
	if err != nil {
		log.Println("@ERR ON COPYING FILE ON UPLOAD: ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	ctx.Response.SetStatusCode(int(Ok))
}

//files is handler for this rest request, routeParts is parts of request path separated by slash
func files(ctx *fasthttp.RequestCtx, routeParts []string) {
	//Reqding client ticket key from request
	ticketKey := string(ctx.Request.Header.Cookie("ticket"))

	uri := ""
	//if roup parts len is 2 or more than save uri and go to reading file,
	//else upload file from request context
	if len(routeParts) > 2 {
		uri = routeParts[2]
	} else {
		uploadFile(ctx)
		return
	}

	//If uri len is more then 3 and icket is valid than continue downloading
	if utf8.RuneCountInString(uri) > 3 && ticketKey != "" {
		//log.Println("@DOWNLOAD")

		//Check if ticket is valid, return fail code if ticket is not valid
		rc, ticket := getTicket(ticketKey)
		if rc != Ok {
			ctx.Response.SetStatusCode(int(rc))
			return
		}

		//Get individual of requested file from tarantool
		rr := conn.Get(true, ticket.UserURI, []string{uri}, false)

		//If common  request code of operation code are not Ok then return fail code
		if rr.CommonRC != Ok {
			log.Println("@ERR COMMON FILES: GET INDIVIDUAL user=", ticket.UserURI, ", uri=", uri)
			ctx.Response.SetStatusCode(int(rr.CommonRC))
			return
		} else if rr.OpRC[0] != Ok {
			ctx.Response.SetStatusCode(int(rr.OpRC[0]))
			return
		}

		//Decode individual with file info and read file info
		fileInfo := BinobjToMap(rr.Data[0])
		//log.Println(fileInfo)
		filePath := fileInfo["v-s:filePath"].([]interface{})[0].(map[string]interface{})
		fileURI := fileInfo["v-s:fileUri"].([]interface{})[0].(map[string]interface{})
		fileName := fileInfo["v-s:fileName"].([]interface{})[0].(map[string]interface{})

		//Create path to file string
		filePathStr := attachmentsPath + filePath["data"].(string) + "/" + fileURI["data"].(string)

		//Check if file exists, return err code if not or InternalServerError if error occured
		_, err := os.Stat(filePathStr)
		if os.IsNotExist(err) {
			ctx.Response.SetStatusCode(int(NotFound))
			return
		} else if err != nil {
			log.Println("@ERR ON CHECK FILE EXISTANCE: ", err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			return
		}

		//Return file to client
		ctx.Response.Header.Set("Content-Disposition", "attachment; filename="+fileName["data"].(string))
		ctx.SendFile(filePathStr)
	}
}
