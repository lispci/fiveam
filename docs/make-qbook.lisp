(asdf:oos 'asdf:load-op :FiveAM)
(asdf:oos 'asdf:load-op :qbook)

(asdf:oos 'qbook:publish-op :fiveam
          :generator (make-instance 'qbook:html-generator
                                    :title "FiveAM"
                                    :output-directory "./html/"))

