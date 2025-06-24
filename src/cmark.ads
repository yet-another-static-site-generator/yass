package CMark is

   function Markdown_To_HTML (Text         : in String;
                              HTML_Enabled : in Boolean)
                              return String;

end CMark;
