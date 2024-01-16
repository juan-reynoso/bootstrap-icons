;;;; bootstrap-icons.lisp

(in-package #:bootstrap-icons)

(defparameter *server* nil)

(defvar *wwwroot*  (concatenate 'string (namestring (asdf:component-pathname (asdf:find-system :bootstrap-icons))) "wwwroot"))

(defun start-web-server (&key (port 8090))
  "Start the web server"
  (setf *server* (start (make-instance 'easy-acceptor :port port))))

(defun stop-web-server ()
  "Stop the web server"
  (stop *server*))

(defun restart-web-server ()
  "Restart the web server"
  (stop-web-server)
  (start-web-server))

(defmacro with-html (title &body body)
  "The basic structure of a web page HTML => HEAD => BODY"
  `(with-yaclml-output-to-string
     (<:html :doctype "html"
	     (<:head
	      (<:meta (@ "charset"
			 "utf-8"))
	      (<:meta :name "viewport"
		      :content "width=device-width, initial-scale=1")
	      (<:link :rel "stylesheet"
		      :type "text/css"
		      :href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css")
	      (<:link :rel "stylesheet"
		      :type "text/css"
		      :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.0/font/bootstrap-icons.css")
	      (<:title (<:as-html ,title)))
	     (<:body
	      ,@body))))

(defun index ()
  "The index webpage"
  (with-html "index"
    (<:div :class "container"
	   (<:div :class "row"
		  (<:div :class "col-12 pb-2"
			 (<:img :class "img-fluid"
				:src "/img/banner.png"))
		  (<:div :class "col-12 pt-4"
			 (<:h1 :class "text-muted"
			       "Hunchentoot and "
			       (<:a :href "https://icons.getbootstrap.com/"
				    "Bootstrap Icons v1.10.0")))
		  (<:div :class "col-11 pb-3"
			 (<:h3 :class "text-success"
			       "Icon font")
			 (<:h5 :class "text-dark"
			       "Icon fonts with classes for every icon are also included for Bootstrap Icons. Include the icon web fonts in your page via CSS, then reference the class names as needed in your HTML.")
			 (<:code "For example: <span class='bi bi-alarm'></span>")
			 (<:div :class "pt-1"
				(<:span :class "bi bi-alarm h1 text-primary")))
		  (<:div :class "col-12 col-md-6 pb-5"
			 (<:div :class "list-group shadow"
				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-files h3 text-primary")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h6 :class "mb-0"
						   "Documents")
					     (<:p :class "mb-0"
						  "Some placeholder content in a paragraph."))))
				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-image-fill h3 text-success")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h6 :class "mb-0"
						   "Photos")
					     (<:p :class "mb-0"
						  "Some placeholder content in a paragraph."))))
				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-film h3 text-dark")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h6 :class "mb-0"
						   "Movies")
					     (<:p :class "mb-0"
						  "Some placeholder content in a paragraph."))))
				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-music-note-beamed h3 text-danger")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h6 :class "mb-0"
						   "Music")
					     (<:p :class "mb-0"
						  "Some placeholder content in a paragraph that goes a little longer so it wraps to a new line."))))
				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-joystick h3 text-secondary")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h6 :class "mb-0"
						   "Games")
					     (<:p :class "mb-0"
						  "Some placeholder content in a paragraph that goes a little longer so it wraps to a new line."))))))))))

(setq *dispatch-table*
      (list
       (create-regex-dispatcher "^/$" 'index)
       (create-prefix-dispatcher  "/index.html" 'index)
       (create-folder-dispatcher-and-handler "/img/" (concatenate 'string *wwwroot* "/img/"))))
