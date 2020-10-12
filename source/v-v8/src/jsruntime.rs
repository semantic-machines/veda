use rusty_v8 as v8;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Once;

pub struct JsRuntime {
    // This is an Option<OwnedIsolate> instead of just OwnedIsolate to workaround
    // an safety issue with SnapshotCreator. See JsRuntime::drop.
    v8_isolate: Option<v8::OwnedIsolate>,
    //snapshot_creator: Option<v8::SnapshotCreator>,
    //has_snapshotted: bool,
    //needs_init: bool,
    //allocations: IsolateAllocations,
}

pub unsafe fn v8_init() {
    let platform = v8::new_default_platform().unwrap();
    v8::V8::initialize_platform(platform);
    v8::V8::initialize();
}

impl JsRuntime {
    pub fn new() -> Self {
        static DENO_INIT: Once = Once::new();
        DENO_INIT.call_once(|| {
            unsafe { v8_init() };
        });

        //let global_context;
        let isolate = {
            let params = v8::Isolate::create_params();

            //params = params.heap_limits(heap_limits.initial, heap_limits.max)

            let isolate = v8::Isolate::new(params);
            let isolate = JsRuntime::setup_isolate(isolate);
            {
                //let scope = &mut v8::HandleScope::new(&mut isolate);
                //let context = bindings::initialize_context(scope);

                //global_context = v8::Global::new(scope, context);
            }
            isolate
        };

        Self {
            v8_isolate: Some(isolate),
            //snapshot_creator: maybe_snapshot_creator,
            //has_snapshotted: false,
            //needs_init: true,
            //allocations: IsolateAllocations::default(),
        }
    }

    fn setup_isolate(mut isolate: v8::OwnedIsolate) -> v8::OwnedIsolate {
        isolate.set_capture_stack_trace_for_uncaught_exceptions(true, 10);
        //isolate.set_promise_reject_callback(bindings::promise_reject_callback);
        //isolate.set_host_initialize_import_meta_object_callback(
        //    bindings::host_initialize_import_meta_object_callback,
        //);
        //isolate.set_host_import_module_dynamically_callback(
        //    bindings::host_import_module_dynamically_callback,
        //);
        isolate
    }

    pub fn global_context(&mut self) -> v8::Global<v8::Context> {
        let state = Self::state(self.v8_isolate());
        let state = state.borrow();
        state.global_context.clone().unwrap()
    }

    pub fn v8_isolate(&mut self) -> &mut v8::OwnedIsolate {
        self.v8_isolate.as_mut().unwrap()
    }

    pub(crate) fn state(isolate: &v8::Isolate) -> Rc<RefCell<JsRuntimeState>> {
        let s = isolate.get_slot::<Rc<RefCell<JsRuntimeState>>>().unwrap();
        s.clone()
    }
}

pub(crate) struct JsRuntimeState {
    pub global_context: Option<v8::Global<v8::Context>>,
}
