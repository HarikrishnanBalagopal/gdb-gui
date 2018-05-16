const {cout, GDBPATH} = require('./util.js');
const WebSocket = require('ws');
const {spawn} = require('child_process');
const fs = require('fs');
const path = require('path');

var global_data = {procs: {}};

function getFileInfo(reqpath, filename)
{
	const fileinfo = fs.statSync(path.join(reqpath, filename));
	return {...fileinfo, datemod: fileinfo.mtime.toDateString(), name: filename, type: path.extname(filename), isDir: fileinfo.isDirectory()};
}

function getDirData(reqpath, next)
{
	fs.readdir(reqpath, (err, filenames) =>
	{
		if(err)
		{
			cout('Error while fs.readdir in handleMessage:', err);
			return;
		}
		const dirdata = filenames.map(filename => getFileInfo(reqpath, filename));
		next(dirdata);
	});
}

function sendData(d, data)
{
	if(d.ws.readyState === 1)d.ws.send(JSON.stringify(data));
}

function handleMessage2(d, msg)
{
	if(typeof msg === 'string')
	{
		const res = JSON.parse(msg);
		cout('debug handleMessage:');
		cout(res);
		if(res.cmd === 'cwd' || res.cmd === 'ls')
		{
			const reqpath = res.cmd === 'cwd' ? process.cwd() : res.path;
			cout('debug handleMessage reqpath:', reqpath);
			getDirData(reqpath, dirdata =>
			{
				const dataToSend = {cmd: res.cmd, path: reqpath, directory: dirdata};
				sendData(d, dataToSend);
			});
		}
		else
		{
			cout('unknown command:', res.cmd);
			return;
		}
	}
	else
	{
		cout('cmd for gdb received:', msg.toString());
		d.procs.gdb.stdin.write(msg);
	}
}

function handleMessage(d, msg)
{
	const res = JSON.parse(msg);
	if(res !== undefined && res.cmd !== undefined)
	{
		cout('debug handleMessage:');
		cout(res);
		if(res.cmd === 'cwd' || res.cmd === 'ls')
		{
			const reqpath = res.cmd === 'cwd' ? process.cwd() : res.path;
			cout('debug handleMessage reqpath:', reqpath);
			getDirData(reqpath, dirdata =>
			{
				const dataToSend = {cmd: res.cmd, path: reqpath, directory: dirdata};
				sendData(d, dataToSend);
			});
		}
		else
		{
			cout('unknown command:', res.cmd);
			return;
		}
	}
	else if(res.gdbCmd !== undefined)
	{
		const gdbCmd = res.gdbCmd + '\n';
		cout('cmd for gdb received:', gdbCmd);
		d.procs.gdb.stdin.write(gdbCmd);
	}
	else
	{
		cout('unknown command:', res);
		return;
	}
}

function init(d)
{
	d.wss = new WebSocket.Server({port: 8080});
	d.wss.on('connection', function connection(ws)
	{
		if(d.procs.gdb)d.procs.gdb.stdin.write('q\n');
		d.ws = ws;
		d.procs.gdb = spawn(GDBPATH, ['--interpreter=mi2']);
		d.procs.gdb.stdout.on('data', data => sendData(d, { gdbRes: data.toString() }));
		d.procs.gdb.stderr.on('data', data => sendData(d, { gdbErr: data.toString() }));
		ws.on('message', msg => handleMessage(d, msg));
		ws.send(JSON.stringify({cmd: 'connected'}));
	});

	cout('initialization complete');
}

init(global_data);