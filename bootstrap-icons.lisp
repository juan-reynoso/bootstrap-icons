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
		      :href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css")
	      (<:link :rel "stylesheet"
		      :type "text/css"		      
		      :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.13.1/font/bootstrap-icons.css")
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
		  (<:div :class "col-12 pt-5"
			 (<:h1 :class "text-muted"
			       "¿Qué es un ataque cibernético?"
			       ))
		  (<:div :class "col-11 pb-3"
			 (<:h3 :class "text-secondary"
			       "Un ataque cibernético es cualquier intento malicioso y deliberado de obtener acceso no autorizado, interrumpir, dañar o deshabilitar sistemas informáticos, redes, dispositivos o datos.")
			 
			 
			 (<:div :class "pt-5 text-danger"
				
				(<:h3 (<:span :class "bi bi-shield-fill-x h1  pe-2")
				 "Propósito y objetivos cumunes")))
		  (<:div :class "col-12 pb-5"
			 (<:div :class "list-group shadow"
				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-shield-lock-fill h3 text-secondary")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h4 :class "mb-0 text-secondary"
						   "Robo de datos")
					     (<:h5 :class "mb-0 text-secondary"
						  "Obtener información sensible como números de tarjetas de crédito, datos personales, propiedad intelectual o secretos comerciales."))))
				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-shield-lock-fill h3 text-danger")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h4 :class "mb-0 text-danger"
						   "Interrupción de Servicios")
					     (<:h5 :class "mb-0 text-primary"
						   "Inhabilitar el funcionamiento normal de un sistema o sitio web, a menudo a través de un ataque de Denegación de Servicio (DoS)"))))
				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-shield-lock-fill h3 text-secondary")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h4 :class "mb-0 text-secondary"
						   "Extorsión")
					     (<:h5 :class "mb-0 text-secondary"
						   "Secuestrar datos o sistemas y exigir un rescate a cambio de su liberación (ransomware)."))))

				(<:a :class "list-group-item list-group-item-action d-flex gap-3 py-3"
				     (<:span :class "bi bi-shield-lock-fill h3 text-secondary")
				     (<:div :class "d-flex gap-2 w-100 justify-content-between"
					    (<:div
					     (<:h4 :class "mb-0 text-secondary"
						   "Espionaje")
					     (<:h5 :class "mb-0 text-secondary"
						   "Obtener acceso a información confidencial de un gobierno, empresa o individuo."))))))
		  (<:h2 :class "pt-5 border-bottom text-primary fw-bold"
			"Ataque de Denegación de Servicio (DoS)")
		  (<:div :class "row g-4 pb-5 row-cols-1 row-cols-lg-3"
			 (<:div :class "col d-flex align-items-start"
				(<:div :class "icon-square d-inline-flex align-items-center justify-content-center fs-4 flex-shrink-0 me-3"
				       (<:span :class "bi bi-tools text-success"))
				(<:div
				 (<:h3 :class "fs-2 text-success"
					"Multiples solicitudes")
				 (<:h5 "El atacante envía una avalancha de solicitudes a la máquina o servidor objetivo.")))

			 (<:div :class "col d-flex align-items-start"
				(<:div :class "icon-square d-inline-flex align-items-center justify-content-center fs-4 flex-shrink-0 me-3"
				       (<:span :class "bi bi-tools text-success"))
				(<:div
				 (<:h3 :class "fs-2 text-success"
					"Tráfico ilegítimo")
				 (<:h5 "El servidor se ve forzado a dedicar todos sus recursos (memoria, CPU, ancho de banda) a manejar este tráfico ilegítimo.")))
			 (<:div :class "col d-flex align-items-start"
				(<:div :class "icon-square d-inline-flex align-items-center justify-content-center fs-4 flex-shrink-0 me-3"
				       (<:span :class "bi bi-tools text-success"))
				(<:div
				 (<:h3 :class "fs-2 text-success"
					"Recursos agotados")
				 (<:h5 "El servidor se ralentiza drásticamente, deja de funcionar o se bloquea por completo, impidiendo que las solicitudes de los usuarios reales y legítimos sean procesadas."
				       ))))

		  (<:h3 :class "text-primary pb-3"
			"El atacante utiliza este método para que el servicio quede denegado a quienes realmente desean utilizarlo.")
		  (<:h2 :class "pt-5 border-bottom text-primary fw-bold"
			"Mitigación")
		  (<:h3 :class "pt-3  text-dark"
			"Mitigar un ataque de Denegación de Servicio (DoS) requiere una combinación de estrategias de prevención, detección y respuesta en diferentes niveles de la infraestructura de red.")
		  (<:style ".feature-icon {
  width: 4rem;
  height: 4rem;
  border-radius: .75rem;
}
.feature-icon-small {
  width: 3rem;
  height: 3rem;
}
")

		  (<:div :class "row row-cols-1 row-cols-sm-2 g-4 pb-5"
			 (<:div :class "col d-flex flex-column gap-2"
				(<:div :class "feature-icon-small d-inline-flex align-items-center justify-content-center text-bg-primary bg-gradient fs-4 rounded-3"
				       (<:span :class "bi bi-code"))
				(<:h4 :class "fw-semibold mb-0 text-body-emphasis"
					     "Comando netstat")
				(<:h5 "Su función principal es mostrar el estado de las conexiones de red, tanto las entrantes como las salientes, así como las estadísticas de los protocolos de red (como TCP, UDP, IP) y las tablas de enrutamiento."))
			 (<:div :class "col d-flex flex-column gap-2"
				(<:div :class "feature-icon-small d-inline-flex align-items-center justify-content-center text-bg-primary bg-gradient fs-4 rounded-3"
				       (<:span :class "bi bi-bricks"))
				(<:h4 :class "fw-semibold mb-0 text-body-emphasis"
					     "Filtrar tráfico con iptables")
				(<:h5 "Decidir qué paquetes de datos deben ser aceptados, rechazados o descartados basándose en criterios como la dirección IP de origen, el puerto, o el protocolo."))

			 (<:div :class "col d-flex flex-column gap-2"
				(<:div :class "feature-icon-small d-inline-flex align-items-center justify-content-center text-bg-primary bg-gradient fs-4 rounded-3"
				       (<:span :class "bi bi-code-slash"))
				(<:h4 :class "fw-semibold mb-0 text-body-emphasis"
				      "Automatización de tareas")
				(<:h5 "Crear script para que ejecute las tareas de mitigación."))
			 (<:div :class "col d-flex flex-column gap-2"
				(<:div :class "feature-icon-small d-inline-flex align-items-center justify-content-center text-bg-primary bg-gradient fs-4 rounded-3"
				       (<:span :class "bi bi-tux"))
				(<:h4 :class "fw-semibold mb-0 text-body-emphasis"
				      "Crear servicio (demonios o daemons)")
				(<:h5 "Es un tipo de programa o proceso que se ejecuta en segundo plano, de forma continua esperando a que ocurran eventos para realizar una función específica.")))))))

(setq *dispatch-table*
      (list
       (create-regex-dispatcher "^/$" 'index)
       (create-prefix-dispatcher  "/index.html" 'index)
       (create-folder-dispatcher-and-handler "/img/" (concatenate 'string *wwwroot* "/img/"))))
