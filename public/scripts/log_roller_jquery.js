$(document).ready(function() {
   	page_load();
});

function page_load() {
	var fullURL = parent.document.URL;
	var index = fullURL.indexOf('server=');
	if (index > -1) {
		var end = fullURL.indexOf('&', index);
		var server = fullURL.substring(index+7, end == -1 ? fullURL.length : end);
		$("#server").attr("value", server);
	} 
	$.get("/servers/" + $("#server").attr("value"), function (tabs) {
		$("#tab_bar").html(tabs);
	});
	$.get("/nodes/" + $("#server").attr("value"), function (nodes) {
		$("#sl_node").html(nodes);
	});
	refresh();
}

function refresh() {
	$("#log_frame").html("");
	
	var params = { 
		server: $("#server").attr("value"),
		type: $("#sl_type").attr("value"), 
		node: $("#sl_node").attr("value"),
		grep: $("#grep").attr("value"),
		max: $("#max").attr("value")
	};
	
	$.ajax({
		url: "/logs",
		global: false,
		type: "POST",
		data: (params),
		dataType: "html",
		success: function(logs) {
		   $("#log_frame").append(logs);
		},
		complete: function(xhr) {
			params['token'] = xhr.getResponseHeader('Token');
		}
	});
	
	fetch_cont(params);
}

function fetch_cont(token, params) {
	var fetch_more = false;
	$.ajax({
		url: "/logs",
		global: false,
		type: "POST",
		data: (params),
		dataType: "html",
		success: function(logs) {
			if(logs.length > 0) {
				$("#log_frame").append(logs);
				fetch_more = true;
			}
		}
	});
	
	if(fetch_more) {
		fetch_cont(token, params);
	}
}

function open_filters() {
	document.getElementById('filter_bar_open').style.display = "block";
	document.getElementById('filter_bar_closed').style.display = "none";
}

function close_filters() {
	document.getElementById('filter_bar_open').style.display = "none";
	document.getElementById('filter_bar_closed').style.display = "block";
}