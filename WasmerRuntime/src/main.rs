extern crate wasmer;

use std::io;
use std::str;
use std::cell::Cell;

use wasmer::{imports, Function, FunctionType, Instance, Module, Store, Type, Value, Memory, MemoryView, LazyInit, WasmerEnv};
use wasmer_compiler_cranelift::Cranelift;
use wasmer_engine_universal::Universal;

use byteorder::{LittleEndian, ReadBytesExt};

fn main() -> Result<(), Box<dyn std::error::Error>> {

    #[derive(WasmerEnv, Clone)]
    pub struct Env {
	#[wasmer(export(alias = "memory"))]
	memory: LazyInit<Memory>,
    }

    let wasm_path = std::env::args().nth(1).expect("no file given");
    // let wasm_bytes = std::fs::read(wasm_path)?;

    // Declare a store
    let store = Store::new(&Universal::new(Cranelift::default()).engine());

    // Lazily init the environment to be able to access memory
    // let env = Env { memory: LazyInit::new() };

    // Compile Wasm module
    println!("Compiling module...");
    let module = Module::from_file(&store, wasm_path)?;

    // Create functions
    let read_signature = FunctionType::new(vec![], vec![Type::I32]);
    let read = Function::new(&store, &read_signature, |_args| {
	println!("Calling `read`");

	let mut input_text = String::new();
	io::stdin()
	    .read_line(&mut input_text)
	    .expect("failed to read from stdin");

	let trimmed = input_text.trim();
	match trimmed.parse::<i32>() {
	    Ok(i) => Ok(vec![Value::I32(i)]),
	    Err(..) => Ok(vec![Value::I32(1)]),// Err(from_trap(lib(BadSignature))),
	}
    });

    let write_signature = FunctionType::new(vec![Type::I32], vec![]);
    let write = Function::new(&store, &write_signature, |args| {
	println!("Calling `write`");

	let result = args[0].unwrap_i32();

	print!("{:?}", result);

	Ok(vec![])
    });

    let writeln_signature = FunctionType::new(vec![], vec![]);
    let writeln = Function::new(&store, &writeln_signature, |_args| {
	println!();

	Ok(vec![])
    });

    let writestr_signature = FunctionType::new(vec![Type::I32], vec![]);
    let writestr = Function::new_with_env
	(&store, &writestr_signature, Env { memory: LazyInit::new() }, |env, args| {
	    let mem = env.memory_ref().unwrap();
	    let ptr = args[0].unwrap_i32() as usize;
	    let view: MemoryView<u8> = mem.view();
	    let size_bytes : Vec<u8> =
		view.get(ptr..ptr+4).unwrap().iter().map(Cell::get).collect();
	    let size =
		size_bytes.as_slice().read_u32::<LittleEndian>().unwrap()
		as usize;
	    let string_start_idx = ptr+8 as usize;
	    let my_string_bytes : Vec<_> =
		view.get(string_start_idx..string_start_idx+size)
		.unwrap()
		.iter()
		.map(Cell::get)
		.collect();
	    // println!("Size: {:?}\n", size);
	    let my_string = str::from_utf8(&my_string_bytes).unwrap();
	    println!("{:?}\n", my_string);
	    Ok(vec![])
	});

    let import_object = imports! {
	"P0lib" => {
	    "read" => read,
	    "write" => write,
	    "writeln" => writeln,
	    "writestr" => writestr,
	}
    };

    // Instantiation
    println!("Instantiating module...");
    let instance = Instance::new(&module, &import_object)?;

    // Call start function and run
    println!("Starting...");
    let start = instance.exports.get_function("program")?;
    start.call(&[])?;

    Ok(())
}
