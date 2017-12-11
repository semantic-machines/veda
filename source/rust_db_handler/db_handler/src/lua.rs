/// This module gives bingings to C lua library
/// Each function name copies macros or function in lua lib
/// Functions are made as safe wrappers around unsafe exports from C

extern crate libc;

use std::os::raw::c_void;
use self::libc::{ ptrdiff_t, size_t };

#[repr(C)]
pub struct lua_State (c_void);
pub type Function = extern "C" fn(L: *mut lua_State) -> i32;

use std;
use std::ffi::CString;
use std::os::raw::c_char;

const LUA_GLOBALSINDEX: i32 = -10002;

extern "C" {
    fn lua_pushcclosure(L: *mut lua_State, func: Function, n: i32);
    fn lua_setfield(L: *mut lua_State, idx: i32, k: *const c_char);
    fn lua_tointeger(L: *mut lua_State, idx: i32) -> ptrdiff_t;
    fn lua_tolstring(L: *mut lua_State, idx: i32, len: *mut size_t) -> *const c_char;
    // void  (lua_pushlstring) (lua_State *L, const char *s, size_t l);
    fn  lua_pushlstring(L: *mut lua_State, s: *const c_char, l: size_t);
} 

#[allow(dead_code)]
pub fn register(L: *mut lua_State, name: &str, func: Function) {
    unsafe {
        lua_pushcclosure(L, func, 0);
        lua_setfield(L, LUA_GLOBALSINDEX, CString::new(name).unwrap().as_ptr());
    }
}

#[allow(dead_code)]
pub fn tointeger(L: *mut lua_State, idx: i32) -> i64 {
    unsafe {
        return lua_tointeger(L, idx) as i64;
    }
}

#[allow(dead_code)]
pub fn tolstring(L: *mut lua_State, idx: i32, buf: &mut Vec<u8>) {
    unsafe {
        let mut str_len: size_t = 0;
        let lstr = lua_tolstring(L, idx, &mut str_len as *mut size_t);
        *buf = std::slice::from_raw_parts(lstr as *const u8, str_len).to_vec();
    }
}

#[allow(dead_code)]
pub fn pushlstring(L: *mut lua_State, buf: &Vec<u8>) {
    unsafe {
        let s = buf[..].as_ptr() as *const c_char;
        lua_pushlstring(L, s, buf.len());
    }
}