function start_bust(e) {
    e.preventDefault();
	data = new Object();
	$.each(["url", "wordlist", "url_restriction"], function(n, e) {
		var value = document.getElementById(e).value;
		if (value) data[e] = value;
	});
	$.each(["follow_dirs", "follow_redirs", "parse_body"], function(n, e) {
		if (document.getElementById(e).checked) data[e] = true;
	});
	$.each(["mangle_found", "postfix"], function(n, e) {
		var value = document.getElementById(e).value;
		if (value) data[e] = value.split(/ *, */);
	});
	$.ajax({
		url: "/bust",
		type: "POST",
		data: JSON.stringify(data),
		contentType: "application/json",
		statusCode: {
			201: function() {
				load_sessions();
			}
		}
	});
}

function load_sessions() {
	$.getJSON("/bust", null, load_sessions_data);
}

function session_detail_clicked(event) {
	update_session_params(event.data);
	update_session_findings(event.data.id);
	$('#detailsModal').modal({});
}

function update_session_findings(id) {
	$.getJSON("/bust/" + id, null, load_session_findings);
}

function load_session_findings(findings) {
	var tbody = $("#detailsModal .detailsFindings tbody").empty();
	findings.sort(function(a, b) { return ((a.url < b.url) ? -1 : ((a.url > b.url) ? 1 : 0)) });
	$(findings).each(function (n, finding) {
		var tr = document.createElement("tr");
		var code = document.createElement("td");
		var url = document.createElement("td");
		var a = document.createElement("a");
		code.appendChild(document.createTextNode(finding.code));
		a.appendChild(document.createTextNode(finding.url));
		if (finding.dir) {
			code.appendChild(document.createTextNode(" "));
			var icon = document.createElement("span");
			icon.className = "glyphicon glyphicon-folder-open";
			icon.title = "directory";
			code.appendChild(icon);
		}
		else if (finding.redir) {
			code.appendChild(document.createTextNode(" "));
			var icon = document.createElement("span");
			icon.className = "glyphicon glyphicon-fast-forward";
			icon.title = "redirection";
			code.appendChild(icon);
		}
		a.href = finding.url;
		url.appendChild(a);
		tr.appendChild(code);
		tr.appendChild(url);
		tbody.append(tr);
	});
}

function update_session_params(config) {
	var settings = {
		url: {
			label: "URL",
			type: "url"
		},
		id: {
			label: "ID",
			type: "string"
		},
		"status": {
			label: "Status",
			type: "status"
		},
		url_restriction: {
			label: "URL restriction",
			type: "string"
		},
		wordlist: {
			label: "Wordlist",
			type: "string"
		},
		postfix: {
			label: "Postfix",
			type: "list"
		},
		mangle_found: {
			label: "Mangling",
			type: "list"
		},
		parse_body: {
			label: "Parse response body",
			type: "flag"
		},
		follow_redirs: {
			label: "Follow redirections",
			type: "flag"
		},
		follow_dirs: {
			label: "Recursively bust directories",
			type: "flag"
		}
	};
	var tbody = $("#detailsModal .detailsParams tbody").empty();
	$.each(settings, function(name, setting) {
		var tr = document.createElement("tr");
		var key = document.createElement("td");
		var value = document.createElement("td");
		key.appendChild(document.createTextNode(setting.label));
		config_value = config[name];
		switch (setting.type) {
			case "flag":
				var icon = document.createElement("span");
				icon.className = "glyphicon glyphicon-" + (config_value ? "ok" : "remove");
				value.appendChild(icon);
				contents = document.createTextNode(" " + (config_value ? "en" : "dis") + "abled");
				break;
			case "string":
				if (config_value == undefined) {
					contents = document.createTextNode("(none)");
					value.className = "text-muted";
				} else {
					contents = document.createElement("code");
					contents.appendChild(document.createTextNode(config_value));
				}
				break;
			case "list":
				if (config_value == undefined) {
					contents = document.createTextNode("(none)");
					value.className = "text-muted";
				} else {
					contents = document.createElement("div");
					$(config_value).each(function(n, item) {
						if (n) contents.appendChild(document.createTextNode(", "));
						code = document.createElement("code");
						code.appendChild(document.createTextNode(item));
						contents.appendChild(code);
					});
				}
				break;
			case "url":
				contents = document.createElement("a");
				contents.href = config_value;
				contents.appendChild(document.createTextNode(config_value));
				break;
			case "status":
				switch (config_value) {
					case "running":
						contents = document.createTextNode("running");
						break;
					case "finished":
						contents = document.createTextNode("finished");
						break;
					case "not_started":
						contents = document.createTextNode("waiting to start");
						break;
					default:
						contents = document.createElement("pre");
						contents.appendChild(document.createTextNode(config_value));
						break;
				}
				break;
		}
		value.appendChild(contents);
		tr.appendChild(key);
		tr.appendChild(value);
		tbody.append(tr);
	});
}

function session_abort_clicked(event) {
	$.ajax({
		url: "/bust/" + event.data.id,
		type: "PUT",
		data: '{"status": "aborted"}',
		contentType: "application/json",
		complete: load_sessions // TODO (re)load after bust_monitor updated the DB
	});
}

function load_sessions_data(sessions) {
	var tbody = $("#sessions tbody").empty();
	sessions.sort(function(a, b) { return ((a.started < b.started) ? 1 : ((a.started > b.started) ? -1 : 0)) });
	$(sessions).each(function (n, session) {
		var tr = document.createElement("tr");
		var id = document.createElement("td");
		var started = document.createElement("td");
		var url = document.createElement("td");
		var a = document.createElement("a");
		var st = document.createElement("td");
		var details = document.createElement("td");
		var button = document.createElement("a");
		id.className = "bust_id";
		id.appendChild(document.createTextNode(session.id));
		started.appendChild(document.createTextNode(session.started));
		a.appendChild(document.createTextNode(session.url));
		a.href = session.url;
		url.appendChild(a);
		tr.appendChild(id);
		tr.appendChild(started);
		tr.appendChild(url);
		switch (session.status) {
			case "running":
				tr.className = "active";
				st.appendChild(document.createTextNode("running"));
				break;
			case "finished":
				tr.className = "success";
				st.appendChild(document.createTextNode("finished at " + session.ended));
				break;
			case "not_started":
				tr.className = "warning";
				st.appendChild(document.createTextNode("waiting to start"));
				break;
			default:
				tr.className = "danger";
				st.appendChild(document.createTextNode("aborted at " + session.ended));
				break;
		}
		tr.appendChild(st);
		button.className = "btn btn-info btn-xs";
		button.appendChild(document.createTextNode("Details"));
		details.appendChild(button);
		$(button).on("click", null, session, session_detail_clicked);
		if (session.status == "running") {
			button = document.createElement("a");
			button.className = "btn btn-danger btn-xs";
			button.appendChild(document.createTextNode("Abort"));
			details.appendChild(document.createTextNode(" "));
			details.appendChild(button);
			$(button).on("click", null, session, session_abort_clicked);
		}
		tr.appendChild(details);
		tbody.append(tr);
	});
}

function load_wordlists() {
	$.getJSON("/wordlists", null, load_wordlists_data);
}

function load_wordlists_data(wordlists) {
	var ul = $("#wldropdown").empty();
	$(wordlists).each(function (n, wordlist) {
		var li = document.createElement("li");
		var a = document.createElement("a");
		a.href = '#';
		a.appendChild(document.createTextNode(wordlist));
		$(a).on("click", null, wordlist, wordlist_selected);
		li.appendChild(a);
		ul.append(li);
	});
}

function wordlist_selected(event) {
	document.getElementById("wordlist").value = event.data;
}

function url_restriction_selected(event) {
	document.getElementById("url_restriction").value = event.data;
}

function url_changed(event) {
	var url = event.target.value.split("/");
	var ul = $("#urdropdown").empty();
	if (url.length < 3) {
		var li = document.createElement("li");
		var a = document.createElement("a");
		a.href = '#';
		a.appendChild(document.createTextNode("(invalid URL)"));
		li.appendChild(a);
		ul.append(li);
	} else {
		var res = new Array();
		var host = url[2].split(":");
		if (host.length == 2) {
			res.push("^" + url[0] + "//" + host[0]);
		}
		for (var i = 3; i <= url.length; i++) {
			res.push("^" + url.slice(0, i).join("/"));
		}
		$(res).each(function(n, re) {
			var li = document.createElement("li");
			var a = document.createElement("a");
			a.href = '#';
			a.appendChild(document.createTextNode(re));
			$(a).on("click", null, re, url_restriction_selected);
			li.appendChild(a);
			ul.append(li);
		});
	}
}

function postfix_selected(event) {
	var postfix = "." + event.data;
	var field = document.getElementById("postfix");
	var idx = field.value.indexOf(postfix);
	if (idx != -1) {
		if (idx == field.value.length - postfix.length) {
			field.value = field.value.replace(new RegExp(
				"[, ]*" + postfix.replace(".", "\\.")), "");
		} else {
			field.value = field.value.replace(new RegExp(
				postfix.replace(".", "\\.") + "[, ]*"), "");
		}
	} else {
		if (field.value) postfix = ", " + postfix;
		field.value += postfix;
	}
	return false;
}

function fill_postfixes() {
	var ul = $("#pfdropdown").empty();
	$("asp,aspx,html,jsp,php".split(",")).each(function (n, postfix) {
		var li = document.createElement("li");
		var a = document.createElement("a");
		a.href = '#';
		a.appendChild(document.createTextNode(postfix));
		$(a).on("click", null, postfix, postfix_selected);
		li.appendChild(a);
		ul.append(li);
	});
}

function mangling_selected(event) {
	var rule = event.data;
	var field = document.getElementById("mangle_found");
	var idx = field.value.indexOf(rule);
	if (idx != -1) {
		if (idx == field.value.length - rule.length) {
			field.value = field.value.replace(new RegExp(
				"[, ]*" + rule.replace("\\", "\\\\").replace(".", "\\.")), "");
		} else {
			field.value = field.value.replace(new RegExp(
				rule.replace("\\", "\\\\").replace(".", "\\.") + "[, ]*"), "");
		}
	} else {
		if (field.value) rule = ", " + rule;
		field.value += rule;
	}
	return false;
}

function fill_manglings() {
	var ul = $("#mfdropdown").empty();
	$("\\1~,.\\1.swp,\\1.old,\\1.orig".split(",")).each(function (n, rule) {
		var li = document.createElement("li");
		var a = document.createElement("a");
		a.href = '#';
		a.appendChild(document.createTextNode(rule));
		$(a).on("click", null, rule, mangling_selected);
		li.appendChild(a);
		ul.append(li);
	});
}

$(function() {
    $("#bust").submit(start_bust);
	load_wordlists();
	load_sessions();
	fill_postfixes();
	fill_manglings();
	url_changed({target: {value: ""}});
	$("#url").on("change", null, null, url_changed);
});
