package main

import (
	"bufio"
	"encoding/hex"
	"hash"
	"hash/crc32"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type QMessageType uint8

const (
	STRING QMessageType = 'S'
	OBJECT QMessageType = 'O'
)

type Mode uint8

const (
	R       Mode = 0
	RW      Mode = 1
	CURRENT Mode = 2
)

const queue_db_path string = "./data/queue"

type Header struct {
	start_pos    uint64
	msg_length   uint64
	count_pushed uint32
	crc          [4]uint8
	_type        QMessageType
}

func (ths *Header) to_buff(buff []uint8) {
	pos := 0

	ulong_to_buff(buff, pos, ths.start_pos)
	pos += 8
	ulong_to_buff(buff, pos, ths.msg_length)
	pos += 8
	uint_to_buff(buff, pos, ths.count_pushed)
	pos += 4
	buff[pos] = uint8(ths._type)
	pos += 1
	buff[pos+0] = 0
	buff[pos+1] = 0
	buff[pos+2] = 0
	buff[pos+3] = 0
}

func (ths *Header) from_buff(buff []uint8) {
	pos := 0

	ths.start_pos = ulong_from_buff(buff, pos)
	pos += 8
	ths.msg_length = ulong_from_buff(buff, pos)
	pos += 8
	ths.count_pushed = uint_from_buff(buff, pos)
	pos += 4
	ths._type = QMessageType(buff[pos])
	pos += 1

	ths.crc[0] = buff[pos+0]
	ths.crc[1] = buff[pos+1]
	ths.crc[2] = buff[pos+2]
	ths.crc[3] = buff[pos+3]
}

func (ths *Header) length() uint64 {
	return 8 + 8 + 4 + 1*4 + 1
}

var buff []uint8
var header_buff []uint8
var buff1 [1]uint8
var buff4 [4]uint8
var buff8 [8]uint8
var crc []uint8

//////////////////////////////////////////// Consumer //////////////////////////////////////////////

type Consumer struct {
	isReady       bool
	queue         *Queue
	name          string
	first_element uint64
	count_popped  uint32
	last_read_msg []uint8

	ff_info_pop_w *os.File
	ff_info_pop_r *os.File

	file_name_info_pop string

	// --- tmp ---
	header Header
	hash   hash.Hash32
}

func NewConsumer(_queue *Queue, _name string) *Consumer {
	p := new(Consumer)
	p.queue = _queue
	p.name = _name
	//tablePolynomial := crc32.MakeTable(0xD5828281)
	//p.hash = crc32.New(tablePolynomial)
	p.hash = crc32.NewIEEE()
	return p
}

func (ths *Consumer) open() bool {
	if !ths.queue.isReady {
		ths.isReady = false
		return false
	}

	ths.file_name_info_pop = queue_db_path + "/" + ths.queue.name + "_info_pop_" + ths.name

	var err error

	if _, err = os.Stat(ths.file_name_info_pop); os.IsNotExist(err) {
		ths.ff_info_pop_w, err = os.OpenFile(ths.file_name_info_pop, os.O_CREATE|os.O_RDWR, 0777)
	} else {
		ths.ff_info_pop_w, err = os.OpenFile(ths.file_name_info_pop, os.O_RDWR, 0777)
	}
	ths.ff_info_pop_r, err = os.OpenFile(ths.file_name_info_pop, os.O_RDONLY, 0777)

	ths.isReady = ths.get_info()

	return ths.isReady
}

func (ths *Consumer) Close() {
	ths.ff_info_pop_w.Sync()
	ths.ff_info_pop_w.Close()
	ths.ff_info_pop_r.Close()
}

func (ths *Consumer) remove() {
	ths.Close()
	os.Remove(ths.file_name_info_pop)
}

func (ths *Consumer) put_info() bool {
	if !ths.queue.isReady || !ths.isReady {
		return false
	}

	var err error
	_, err = ths.ff_info_pop_w.Seek(0, 0)

	if err == nil {
		_, err = ths.ff_info_pop_w.WriteString(ths.queue.name + ";" + strconv.FormatInt(int64(ths.queue.chunk), 10) + ";" + ths.name + ";" + strconv.FormatUint(ths.first_element, 10) + ";" + strconv.FormatUint(uint64(ths.count_popped), 10))

		if err == nil {
			err = ths.ff_info_pop_w.Sync()
		}

	}

	if err != nil {

		//	       log.trace("consumer:put_info [%s;%d;%s;%d;%d] %s", queue.name, queue.chunk, name, first_element, count_popped, tr.msg);
		return false
	}

	return true
}

func (ths *Consumer) get_info() bool {
	if !ths.queue.isReady {
		return false
	}

	var err error

	ths.ff_info_pop_r.Seek(0, 0)
	rr := bufio.NewReader(ths.ff_info_pop_r)
	str, err := Readln(rr)

	if str != "" && err == nil {
		ch := strings.Split(str[0:len(str)-1], ";")
		if len(ch) != 5 {
			ths.isReady = false
			return false
		}

		_name := ch[0]
		if _name != ths.queue.name {
			//log.trace("consumer:get_info:queue name from info [%s] != consumer.queue.name[%s]", _name, queue.name)
			ths.isReady = false
			return false
		}

		var _chunk int64
		_chunk, err = strconv.ParseInt(ch[1], 10, 0)
		if int32(_chunk) != ths.queue.chunk {
			//log.trace("consumer:get_info:queue chunk from info [%d] != consumer.queue.chunk[%d]", _chunk, queue.chunk)
			ths.isReady = false
			return false
		}

		_name = ch[2]
		if _name != ths.name {
			//log.trace("consumer:get_info:consumer name from info[%s] != consumer.name[%s]", _name, name)
			ths.isReady = false
			return false
		}

		nn, err := strconv.ParseInt(ch[3], 10, 0)

		if err != nil {
			ths.isReady = false
			return false
		}

		ths.first_element = uint64(nn)

		nn, err = strconv.ParseInt(ch[4], 10, 0)
		if err != nil {
			ths.isReady = false
			return false
		}

		ths.count_popped = uint32(nn)
	}

	//log.trace("get_info:%s", text(this))

	return true
}

func (ths *Consumer) pop() string {

	if !ths.queue.isReady || !ths.isReady {
		return ""
	}

	if ths.count_popped >= ths.queue.count_pushed {
		return ""
	}
	ths.queue.ff_queue_r.Seek(int64(ths.first_element), 0)

	ths.queue.ff_queue_r.Read(header_buff)
	ths.header.from_buff(header_buff)

	if ths.header.start_pos != ths.first_element {
		//log.trace("pop:invalid msg: header.start_pos[%d] != first_element[%d] : %s", header.start_pos, first_element, text(header));
		return ""
	}
	//        writeln("@queue=", this);
	//        writeln("@header=", header);

	if ths.header.msg_length >= uint64(len(buff)) {
		//log.trace("pop:inc buff size %d -> %d", buff.length, header.msg_length);
		buff = make([]uint8, ths.header.msg_length+1)
	}

	if ths.header.msg_length < uint64(len(buff)) {
		ths.queue.ff_queue_r.Read(buff[0:ths.header.msg_length])

		ths.last_read_msg = make([]uint8, ths.header.msg_length)

		copy(ths.last_read_msg, buff[0:ths.header.msg_length])
		if uint64(len(ths.last_read_msg)) < ths.header.msg_length {
			//log.trace("pop:invalid msg: msg.length < header.msg_length : %s", text(header));
			return ""
		}
	} else {
		//log.trace("pop:invalid msg: header.msg_length[%d] < buff.length[%d] : %s", header.msg_length, buff.length, text(header));
		return ""
	}

	return string(ths.last_read_msg)
}

func (ths *Consumer) commit_and_next() bool {
	if !ths.queue.isReady || !ths.isReady {
		//log.trace("ERR! queue:commit_and_next:!queue.isReady || !isReady");
		return false
	}

	if ths.count_popped >= ths.queue.count_pushed {
		//log.trace("ERR! queue[%s][%s]:commit_and_next:count_popped(%d) >= queue.count_pushed(%d)", queue.name, name, count_popped,
		//          queue.count_pushed);
		return false
	}

	header_buff[0] = 0
	header_buff[1] = 0
	header_buff[2] = 0
	header_buff[3] = 0

	ths.hash.Reset()
	ths.hash.Write(header_buff)
	ths.hash.Write(ths.last_read_msg)
	hashInBytes := ths.hash.Sum(nil)[:]
	crc = []uint8(hex.EncodeToString(hashInBytes))

	if ths.header.crc[0] != crc[0] || ths.header.crc[1] != crc[1] || ths.header.crc[2] != crc[2] || ths.header.crc[3] != crc[3] {
		//log.trace("ERR! queue:commit:invalid msg: fail crc[%s] : %s", text(crc), text(header));
		//log.trace(text(last_read_msg.length));
		//log.trace(cast(string)last_read_msg);
		return false
	}

	ths.count_popped++
	ths.first_element += ths.header.length() + ths.header.msg_length

	return ths.put_info()
}

//////////////////////////////////////////// Queue //////////////////////////////////////////////

type Queue struct {
	isReady      bool
	name         string
	chunk        int32
	right_edge   uint64
	count_pushed uint32
	mode         Mode

	ff_info_push_w *os.File
	ff_info_push_r *os.File

	ff_queue_w *os.File
	ff_queue_r *os.File

	file_name_info_push string
	file_name_queue     string
	file_name_lock      string

	// --- tmp ---
	header Header
	hash   hash.Hash32
}

func NewQueue(_name string, _mode Mode) *Queue {
	p := new(Queue)

	p.name = _name
	p.mode = _mode

	p.isReady = false
	buff = make([]uint8, 4096*100)
	header_buff = make([]uint8, p.header.length())

	p.file_name_info_push = queue_db_path + "/" + p.name + "_info_push"
	p.file_name_queue = queue_db_path + "/" + p.name + "_queue_" + strconv.Itoa(int(p.chunk))
	p.file_name_lock = queue_db_path + "/" + p.name + "_queue.lock"

	p.hash = crc32.NewIEEE()
	return p
}

func (ths *Queue) open(_mode Mode) bool {

	if ths.isReady == false {
		if _mode != CURRENT {

			ths.mode = _mode
		}
	}

	var err error
	//defer log.trace("ERR! queue, not open: ex: %s", ex.msg);

	//writeln("open ", text (mode));

	if ths.mode == RW {
		if _, err = os.Stat(ths.file_name_lock); os.IsNotExist(err) == false {
			//log.trace("Queue [%s] already open, or not deleted lock file", name)
			return false
		}
		err = ioutil.WriteFile(ths.file_name_lock, []byte("0"), 0644)

		if _, err = os.Stat(ths.file_name_info_push); os.IsNotExist(err) {
			ths.ff_info_push_w, err = os.OpenFile(ths.file_name_info_push, os.O_CREATE|os.O_RDWR, 0777)
		} else {
			ths.ff_info_push_w, err = os.OpenFile(ths.file_name_info_push, os.O_RDWR, 0777)
		}

		if err != nil {
			return false
		}

		if _, err = os.Stat(ths.file_name_queue); os.IsNotExist(err) {
			ths.ff_queue_w, err = os.OpenFile(ths.file_name_info_push, os.O_CREATE|os.O_RDWR|os.O_APPEND, 0777)
		} else {
			ths.ff_queue_w, err = os.OpenFile(ths.file_name_info_push, os.O_RDWR|os.O_APPEND, 0777)
		}

		if err != nil {
			return false
		}

		ths.ff_info_push_r, err = os.OpenFile(ths.file_name_info_push, os.O_RDONLY, 0777)

		if err != nil {
			return false
		}
		ths.ff_queue_r, err = os.OpenFile(ths.file_name_queue, os.O_RDONLY, 0777)

		if err != nil {
			return false
		}

		ths.isReady = true
		ths.get_info()

		var queue_r_info os.FileInfo

		queue_r_info, err = os.Stat(ths.file_name_queue)

		if ths.mode == R && queue_r_info.Size() < int64(ths.right_edge) || ths.mode == RW && queue_r_info.Size() != int64(ths.right_edge) {
			ths.isReady = false
			//log.trace("ERR! queue:open(%s): [%s].size (%d) != right_edge=", text(mode), file_name_queue, ff_queue_r.size(), right_edge);
		} else {
			ths.isReady = true
			ths.put_info()
		}

	}
	return ths.isReady
}

func (ths *Queue) get_info() bool {

	if !ths.isReady {
		return false
	}

	var err error

	ths.ff_info_push_r.Seek(0, 0)
	//        writeln("@2 ff_info_push_r.size=", ff_info_push_r.size);

	rr := bufio.NewReader(ths.ff_info_push_r)
	str, err := Readln(rr)

	//writeln("@3 str=[", str, "]");
	if str != "" && err == nil {
		ch := strings.Split(str[0:len(str)-1], ";")
		if len(ch) != 5 {
			ths.isReady = false
			return false
		}

		if ch[0] != ths.name {
			ths.isReady = false
			return false
		}
		ths.name = ch[0]

		var chunk int64
		chunk, err = strconv.ParseInt(ch[1], 10, 0)

		ths.chunk = int32(chunk)

		var right_edge int64
		right_edge, err = strconv.ParseInt(ch[2], 10, 0)

		ths.right_edge = uint64(right_edge)

		var count_pushed int64
		count_pushed, err = strconv.ParseInt(ch[3], 10, 0)

		ths.count_pushed = uint32(count_pushed)
		//string hash_hex = ch[ 4 ];
	}

	//writeln(this);

	return true
}

func (ths *Queue) put_info() {
	if !ths.isReady || ths.mode == R {
		return
	}
	ths.ff_info_push_w.Seek(0, 0)

	data := ths.name + ";" + strconv.FormatInt(int64(ths.chunk), 10) + ";" + strconv.FormatInt(int64(ths.right_edge), 10) + ";" + strconv.FormatUint(uint64(ths.count_pushed), 10)

	ths.hash.Reset()
	ths.hash.Write(data)
	hashInBytes := ths.hash.Sum(nil)[:]
	hash_hex := []uint8(hex.EncodeToString(hashInBytes))

	ths.ff_info_push_w.Write(data)
	ths.ff_info_push_w.Writeln(hash_hex)
}
