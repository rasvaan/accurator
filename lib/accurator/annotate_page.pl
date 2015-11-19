:- module(annotate_page, [http_image_annotation/1]).

/** <module> Image annotation page
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(user(user_db)).
:- use_module(applications(annotation)).

http_image_annotation(Request) :-
	get_annotation_parameters(Request, Options),
	reply_page(Options).

get_annotation_parameters(Request, Options) :-
	http_parameters(Request,
	[ uri(Uri,
		  [uri, description('URI of the object to be annotated') ])
	]),
	Options = [uri(Uri)].

reply_page(_Options) :-
	% Sent to intro page when not logged in
	not(logged_on(_User)),
	reply_html_page(
		[title('Not logged in'),
		 meta([name('viewport'),content('width=device-width, initial-scale=1.0')])
		],
		[script(type('text/javascript'), 'document.location.href="intro.html"')]).

reply_page(Options) :-
    option(uri(Uri), Options),
	logged_on(User),
	get_annotation_ui(User, UI),
	get_anfields(UI, [], [], AnnotationFields),
	debug(anno, 'UI: ~p', [UI]),
	AnnotationOptions = [targets([Uri]),
						 ui(UI),
						 annotation_fields(AnnotationFields),
						 metadata_fields(['http://semanticweb.cs.vu.nl/annotate/ui/imageURL']),
						 user(User)],
	http_absolute_location(cliopatria('img/favicon.ico'), LogoUrl, []),
	reply_html_page(
	[title(Uri),
	 link([href(LogoUrl),rel('shortcut icon')]),
	 meta([name('viewport'),content('width=device-width, initial-scale=1.0')])
    ],
	[\navigation_bar,
	  div([ class('container')],
		  [\events,
		   \annotation_page_body(AnnotationOptions),
		   \navigation,
		   \metadata,
		   \footer,
		   \login_modal,
		   \annotate_javascript,
		   \html_requires(css('bootstrap.min.css')),
		   \html_requires(css('search.css')),
		   \html_requires(css('accurator.css'))])]).

get_annotation_ui(User, UI) :-
	user_property(User, domain(Domain)),
	atom_string(DomainAtom, Domain),
	atomic_list_concat(['http://accurator.nl/', DomainAtom, '#domain'], DomainUri),
	rdf(DomainUri, accu:hasAnnotationUI, UI).

%%	navigation_bar(+Page)
%
%	Generates the navigation bar.
navigation_bar -->
    html({|html||
	<!-- Navbar -->
	<nav class="navbar navbar-inverse navbar-fixed-top" role="navigation">
		 <div class="container-fluid">
			<div class="navbar-header">
				<button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbarDivMenu">
					<span class="icon-bar"></span>
					<span class="icon-bar"></span>
				</button>
				<a class="navbar-brand" href="intro.html">
					<img id="navbarImgLogo" src="img/accurator.png" alt="Accurator">
				</a>
			</div>
			<div class="collapse navbar-collapse" id="navbarDivMenu">
			        <ul class="nav navbar-nav navbar-right navbarLstFlag">
				</ul>
	                        <ul class="nav navbar-nav navbar-right navbarLstUser">
				</ul>
				<div class="navbar-form navbar-nav" id="navbarFrmSearch">
					<div class="form-group">
						<input type="text" class="form-control" id="navbarInpSearch">
					</div>
					<button id="navbarBtnSearch" class="btn btn-default">
					</button>
				</div>
				<button class="btn navbar-btn navbar-right btn-primary" id="navbarBtnRecommend">
				</button>
			</div>
		</div>
	</nav>
	|}).

annotate_javascript -->
    html({|html||
	<!-- Added Script -->
	<script type="text/javascript" src="js/accurator.jquery.min.js"></script>
	<script type="text/javascript" src="js/accurator.bootstrap.min.js"></script>
	<script type="text/javascript" src="js/accurator.laconic.js"></script>
	<script type="text/javascript" src="js/pengines.js"></script>
	<script type="text/javascript" src="js/result.js"></script>
	<script type="text/javascript" src="js/search.js"></script>
	<script type="text/javascript" src="js/accurator_utilities.js"></script>
	<script type="text/javascript" src="js/accurator_annotate.js"></script>
	<script>annotateInit()</script>
	|}).
navigation -->
	html({|html||
	<!-- Navigation -->
	<div class="row" id="clusterNavigation">
	    <div class="navigationButton col-md-2 col-xs-6">
			<button class="btn btn-default navButton" id="btnPrevious">
		        <span class="glyphicon glyphicon-chevron-left"></span>
			</button>
		</div>
		<div class="navigationButton col-md-2 col-xs-6 col-md-push-8" id="btnAlignRight">
			<button class="btn btn-default navButton" id="btnNext">
		        <span class="glyphicon glyphicon-chevron-right"></span>
			</button>
		</div>
		<div class="col-md-8 col-md-pull-2" id="path">
		</div>
	</div>
	|}).
events -->
	html({|html||
	<!-- Events -->
	<div class="container" id="events">
	</div>
	|}).

metadata -->
	html({|html||
	<!-- Metadata -->
	<div id="metadata"></div>
	|}).
login_modal -->
    html({|html||
	<!-- Login modal -->
	<div class="modal fade" id="loginDivLogin">
		<div class="modal-dialog">
			<div class="modal-content">
				<div class="modal-header">
					<button type="button" class="close" id="loginBtnClose">&times;</button>
					<h4 id="loginHdrTitle">
					</h4>
				</div>
				<div class="modal-body">
					<form role="form">
						<div class="form-group">
							<label id="loginLblUsername" for="loginInpUsername">
							</label>
							<input type="text" class="form-control" id="loginInpUsername">
						</div>
						<div class="form-group">
							<label id="loginLblPassword" for="password">
							</label>
							<input type="password" class="form-control" id="loginInpPassword">
						</div>
						<p class="text-warning" id="loginTxtWarning">
						</p>
					</form>
				</div>
				<div class="modal-footer">
					<button class="btn btn-primary" id="loginBtnLogin">
					</button>
				</div>
			</div>
		</div>
	</div>
	|}).

footer  -->
    html({|html||
		<!-- Logos -->
		<div class="footerRelative">
			<div class="logo col-md-2 col-xs-4">
				<a href="http://commit-nl.nl/">
					<img src="img/logos/commit.png" class="footerLogo" id="commitFooterLogo" alt="Logo of " />

				</a>
			</div>
			<div class="logo col-md-2 col-xs-4">
				<a href="http://www.cwi.nl">
					<img src="img/logos/cwi.png" class="footerLogo" alt="Logo of CWI" />
				</a>
			</div>
			<div class="logo col-md-2 col-xs-4">
				<a href="http://www.tudelft.nl">
					<img src="img/logos/tud.png" class="footerLogo" alt="Logo of TU Delft" />
				</a>
			</div>
			<div class="logo col-md-2 col-xs-4">
				<a href="http://vu.nl/">
					<img src="img/logos/vu.png" class="footerLogo" alt="Logo of VU University Amsterdam" />
				</a>
			</div>
			<div class="logo col-md-2 col-xs-4">
				<a href="http://rijksmuseum.nl">
					<img src="img/logos/rma.png" class="footerLogo" id="rijksmuseumLogo" alt="Logo of Rijksmuseum" />
				</a>
			</div>
			<div class="logo col-md-2 col-xs-4">
				<a href="http://www.naturalis.nl/">
					<img src="img/logos/naturalis.png" class="footerLogo" alt="Logo of Naturalis" />
				</a>
			</div>
		</div>
		 |}).
