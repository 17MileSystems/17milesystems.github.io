;;--------------------=={ SBUpdate }==---------------------;;
;;                                                                      ;;
;;  This program automatically resets dynamic scale bar blocks to       ;;
;;  match specified attributes.                                         ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Trevor Loney, Copyright © 2024  -  www.17Mile.system       ;;
;;----------------------------------------------------------------------;;
;;  Version 0.9    -    2024-07-04                                      ;;
;;----------------------------------------------------------------------;;

;;	The SBUPDATE function searches all ACAD support paths and build a list of all SBU.config objects

;;
;;  SBU.config format - Json format, list the:
;;    AttributeBlockName, Attributes, ScaleBarBlock, 
;;    and optionally ScaleBarOffset (if required, default [0,0]) 
;;    and ScaleBarScale (if required, default 1.0)
;;
;;  [
;;	  {
;;      "AttributeBlockName":"MRR_Detail_A1_Att",
;;      "Attributes": ["SCALE_1", "SCALE_2", "SCALE_3" ],
;;      "ScaleBarBlock": "ScaleBar_Dynamic",
;;      "ScaleBarOffset": [0, 0],
;;      "ScaleBarScale": 1.0,
;;    },
;;	  {
;;      "AttributeBlockName":"MRR_Detail_A3_Att",
;;      "Attributes": ["SCALE_1", "SCALE_2", "SCALE_3" ],
;;      "ScaleBarBlock": "ScaleBar_Dynamic",
;;      "ScaleBarOffset": [0, 0],
;;      "ScaleBarScale": 1.0,
;;    },
;;    ... <-- as many definitions as required.
;;  ]
;;  

(defun C:SBUPDATE
	( 
		/ 
		configs 
		lex_string
		lex_number
		contains
		tokenise
		parse_object
		parse_array
		parseTokens
		fromJson 
		loadFile 
		SBUReadConfig 
		SBUReadAllConfig 
		get_at_list 
		getvisibilityparametername 
		getdynpropallowedvalues
		setdynpropvalue
		SetVisibilityState
	)
	(princ "\nCopyright © 2024 - www.17Mile.system")
	(defun lex_string(string / JSON_QUOTE JSON_BACKSLASH c)
		(setq JSON_QUOTE "\"" JSON_BACKSLASH "\\")
		(if (eq (substr string 1 1) JSON_QUOTE)
			(progn
				(setq c 2)
				(while 
					(and 
						(<= c (strlen string))
						(not (eq (substr string c 1) JSON_QUOTE))
					)
					(if (eq (substr string c 1) JSON_BACKSLASH)
						(setq c (+ c 2))
						(setq c (+ c 1))
					)
				)
				(if (eq (substr string c 1) JSON_QUOTE)
					(list (substr string 2 (- c 2)) (substr string (+ c 1)))
					(list nil string)
				)			
			)
			(list nil string)
		)
	)

	(defun lex_number(string / c )
		(setq JSON_NUMBER "-0123456789.")
		(if (contains (substr string 1 1) JSON_NUMBER)
			(progn
				(setq c 1)
				(while 
					(and 
						(<= c (strlen string))
						(contains (substr string c 1) JSON_NUMBER)
					)
					(setq c (+ c 1))
				)
				(list (substr string 1 (- c 1)) (substr string c))
			)
			(list nil string)
		)
	)

	(defun contains (character string / doesContains )
		(setq doesContains nil)
		(while (and (> (strlen string) 0) (not doesContains))
			(if (eq (substr string 1 1) character)
				(setq doesContains T)
				(setq string (substr string 2))
			)
		)
		doesContains
	)

	(defun tokenise(string / JSON_QUOTE JSON_WHITESPACE JSON_SYNTAX JSON_NUMBER tokens number token)
		(setq JSON_QUOTE "\"'"
			JSON_WHITESPACE " \t\n\r"
			JSON_SYNTAX "[]{}:,"
			JSON_NUMBER "-0123456789."
		)
		(setq tokens nil)
		(while (> (strlen string) 0)
			(setq c (substr string 1 1))
			(if (contains c JSON_WHITESPACE)
				(progn
					;;(princ "\nWHITESPACE")
					(setq string (substr string 2))
				)
				(if (contains c JSON_SYNTAX)
					(progn
						;; (princ "\nJSON_SYNTAX")
						(setq tokens (append tokens (list c)) string (substr string 2))
					)
					(if (contains c JSON_NUMBER)
						(progn
							;;(princ "\nJSON_NUMBER")
							(setq number (lex_number string) num (car number) tokens (append tokens (list (if(eq (atoi num) (atof num))(atoi num)(atof num)))) string (cadr number))
						)
						(if (contains c JSON_QUOTE)
							(progn
								;;(princ "\nJSON_QUOTE")
								(setq token (lex_string string) tokens (append tokens (list (car token))) string (cadr token))
							)
							(progn
								;; (princ "\nBADVALUE")
								(setq tokens (append tokens (list nil)) 
									string (substr string 2)
								)
							)
						)
					)
				)
			)
		)
		tokens
	)
	
	(defun parse_object(tokens / json_object key colon pTokens json)
		(setq json_object nil )
		(while (and (> (length tokens) 0) (not (eq (car tokens) "}")))
			(setq key (car tokens)
				colon (cadr tokens)
				pTokens (parseTokens (cddr tokens))
				json (car pTokens)
				tokens (cadr pTokens)
			)
			(if (eq (car tokens) ",") (setq tokens (cdr tokens)))
			(setq json_object (append json_object (list (cons key json))))
		)
		(list json_object (cdr tokens))
	)

	(defun parse_array(tokens / json JSON_RIGHTBRACKET JSON_COMMA json_array token)
		
		(setq JSON_RIGHTBRACKET "]"
			JSON_COMMA ","
			json_array nil
			token (car tokens)
		)
		(while (and (> (length tokens) 0) (not (eq (car tokens) JSON_RIGHTBRACKET)))
			(progn
				(setq pTokens (parseTokens tokens) 
					json (car pTokens) 
					tokens (cadr pTokens)
					json_array (append json_array (list json))
					token (car tokens)
				)
				(if (eq token JSON_COMMA) 
					(setq token (car tokens) tokens (cdr tokens))
					(if (eq token JSON_RIGHTBRACKET)
						nil
						(setq tokens nil)
					)
				)
			)
		)
		(list json_array (cdr tokens))
	)
	
	(defun parseTokens(tokens / token)
		(setq JSON_LEFTBRACKET "["
			JSON_LEFTBRACE "{"
		)
		(setq token (car tokens))
		(if (eq token JSON_LEFTBRACKET)
			(parse_array (cdr tokens))
			(if (eq token JSON_LEFTBRACE)
				(parse_object (cdr tokens))
				(list token (cdr tokens))
			)
		)
	)
	;; fromJson - Tokenise and parse list from json string
	(defun fromJson(string)
		(car (parseTokens (tokenise string)))
	)

	(defun loadFile(path / filehandle lines)
		(setq filehandle (open path "r"))
		(setq lines nil)
		(while (setq line (read-line filehandle))
			(if lines
				(setq lines (strcat lines "\n" line))
				(setq lines line)
			)
			
		)
		(close filehandle)
		lines
	)

	(defun SBUReadConfig ( config / des lst sep str config2list)
	  (fromJson (loadFile config))
	)

	(defun SBUReadAllConfig( / SupportPaths ConfigFiles Configs settings)
		(defun SplitStr ( s d / p ) (if (setq p (vl-string-search d s))(cons (substr s 1 p) (SplitStr (substr s (+ p 1 (strlen d))) d)) (list s)))	
		(setq SupportPaths (SplitStr (vl-registry-read (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\Profiles\\" (getvar "CPROFILE") "\\General") "ACAD") ";"))
		(setq ConfigFiles (vl-remove 'nil (mapcar (quote  (lambda (a) (findfile (strcat a "\\SBU.config")))) SupportPaths)))	
		(setq Configs nil)
		(foreach ConfigFile ConfigFiles 
			(setq settings (SBUReadConfig ConfigFile)
				Configs	(append Configs settings)
			)
		)
		Configs
	)
	
	(defun get_at_list(blname / atlist attslist bldata blflag )
	 (setq atlist nil attslist nil)
	 (if blname ; if you sent parameter
	  (if (= (type blname)(quote ename)) ; if it is an entity name
	   (if (= "INSERT" (cdr (assoc 0 (setq bldata (entget blname))))) ; if insert
		(if (setq blflag (cdr (assoc 66 bldata))) ; if it has atts follow flag
		 (if (= 1 blflag) ; if atts follow flag indicates atts
		  (while
		   (=
			"ATTRIB"
			(cdr (assoc 0 (setq bldata (entget (setq blname (entnext blname))))))
		   )
		   (setq atlist (cons (list (strcase (cdr (assoc 2 bldata))) bldata) atlist))
		  )
		 )
		)
	   )
	  )
	 )
	 (foreach attent atlist 
		(setq atts (list 
			(car attent) 
			(cdr (assoc 1 (cadr attent))) 
			(cdr (assoc 10 (cadr attent))) 
		))
		(setq attslist (append attslist (list atts)))
	 )
	 attslist
	)
	(defun getvisibilityparametername ( blk / vis )  
		(if
			(and
				(vlax-property-available-p blk 'effectivename)
				(setq blk
					(vla-item
						(vla-get-blocks (vla-get-document blk))
						(vla-get-effectivename blk)
					)
				)
				(= :vlax-true (vla-get-isdynamicblock blk))
				(= :vlax-true (vla-get-hasextensiondictionary blk))
				(setq vis
					(vl-some
					   '(lambda ( pair )
							(if
								(and
									(= 360 (car pair))
									(= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
								)
								(cdr pair)
							)
						)
						(dictsearch
							(vlax-vla-object->ename (vla-getextensiondictionary blk))
							"ACAD_ENHANCEDBLOCK"
						)
					)
				)
			)
			(cdr (assoc 301 (entget vis)))
		)
	)
		
	(defun getdynpropallowedvalues ( blk prp )
		(setq prp (strcase prp))
		(vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
			(vlax-invoke blk 'getdynamicblockproperties)
		)
	)
		
	(defun setdynpropvalue ( blk prp val )
		(setq prp (strcase prp))
		(vl-some
		   '(lambda ( x )
				(if (= prp (strcase (vla-get-propertyname x)))
					(progn
						(vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
						(cond (val) (t))
					)
				)
			)
			(vlax-invoke blk 'getdynamicblockproperties)
		)
	)
		
	(defun SetVisibilityState ( blk val / vis )
		(if
			(and
				(setq vis (getvisibilityparametername blk))
				(member (strcase val) (mapcar 'strcase (getdynpropallowedvalues blk vis)))
			)
			(setdynpropvalue blk vis val)
		)
	)
	
	(setq configs (SBUReadAllConfig))
	(foreach config configs
		(progn 
			(setq conOut config)
			(setq blockName (cdr (assoc "AttributeBlockName" config)))
			(setq attributeNames (cdr (assoc "Attributes" config)))
			(setq ScaleBarBlockName (cdr (assoc "ScaleBarBlock" config)))
			(setq InsertOffset (cdr (assoc "ScaleBarOffset" config)))
			(setq InsertScale (cdr (assoc "ScaleBarScale" config)))
			
			;; default values
			(if (not InsertOffset) (setq InsertOffset (list 0 0)))
			(if (not InsertScale) (setq InsertScale 1.0))
			
			(if blockName
				(progn
					(setq ss (ssget "X" (list (cons 0 "insert")(cons 2 blockName))))
					(if ss
						(progn 
							(setq ent (ssname ss 0))
							(setq attslist (get_at_list ent))
							(setq ssDynamics (ssget "_X" (list (cons 0 "INSERT")(cons 2 (strcat "`*U*,"  ScaleBarBlockName)))))
							(setq i 0)
							(repeat (sslength ssDynamics)
								(progn
									(entdel (ssname ssDynamics i))
									(setq i(1+ i))
								)
							)
							(foreach attributeName attributeNames
								(setq props (assoc attributeName attslist))
								(setq attvalue (cadr props))
								(setq attLocation (caddr props))
								(progn 
									(setq blockLocation (list (+ (car attLocation) (car InsertOffset)) (+ (cadr attLocation) (cadr InsertOffset))))
									(setq entList (list (cons 0 "INSERT")(cons 2 ScaleBarBlockName)(cons 10 blockLocation) (cons 41 InsertScale) (cons 42 InsertScale) (cons 43 InsertScale)))
									(if (setq entList (entmake entList))
										(if (setq newEnts (ssget "X" entList))
											(progn 
												(setq ent (ssname newEnts 0))
												(setq result (SetVisibilityState (vlax-ename->vla-object ent) attvalue))
												(if (not result)
													(entdel ent)
												)
											)
										)
									) 
								)
								
							)
						)
						(progn
							(princ (strcat "\n" blockName " - Not Found"))
						)
					)
				)
			)
		)
	)
	(princ)
)
; Called after Command Ends
(defun M17:endCommand (calling-reactor endcommandInfo / )
	(if (member (nth 0 endcommandInfo) (LIST "EATTEDIT" "UPDATETITLEBLOCKFROMFILE" "TITLEBLOCKUPDATE" "TBUPDATEONOPEN" "TBU" "UPDATE"))
		(vla-sendcommand (vla-get-activedocument (vlax-get-acad-object)) "SBUPDATE\n")
	)
	(princ)
)
; Command Rector For End Command
(vlr-command-reactor nil '((:vlr-commandEnded . M17:endCommand)))
(princ)
