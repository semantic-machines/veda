package main

import (
	"log"
	"strconv"

	"github.com/valyala/fasthttp"
)

//getOperationState readds state of requested operation in requested module
func getOperationState(ctx *fasthttp.RequestCtx) {
	//Reading parametrs from request context
	moduleID, _ := ctx.QueryArgs().GetUint("module_id")
	waitOpID, _ := ctx.QueryArgs().GetUint("wait_op_id")

	//Looking for module info structure in cache
	//If not found than open new one
	mif := mifCache[moduleID]
	if mif == nil {
		moduleName := ModuleToString(Module(moduleID))
		mif = NewModuleInfoFile(moduleName, Reader)
		mifCache[moduleID] = mif
	}

	//Reading info, set res -1 for fail code
	info := mif.GetInfo()
	res := int64(-1)
	//If read successfully then set res to operation id
	if info.IsOk {
		if Module(moduleID) == fulltext_indexer || Module(moduleID) == scripts_main {
			res = info.CommittedOpID
		} else {
			res = info.OpID
		}
	}
	log.Printf("get_operation_state(%d) info=[%v], wait_op_id=%d\n", moduleID, info, waitOpID)
	ctx.Write([]byte(strconv.FormatInt(res, 10)))
}
