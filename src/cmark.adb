
with Interfaces.C.Strings;

package body CMark is

   use Interfaces.C;

   subtype size_t is unsigned_long;

   ------------------------------
   -- C_CMark_Markdown_to_HTML --
   ------------------------------

   function C_CMark_Markdown_To_HTML
     (Text    : Strings.chars_ptr;
      Len     : size_t;
      Options : int)
      return Strings.chars_ptr
   with
      Import        => True,
      Convention    => C,
      External_Name => "cmark_markdown_to_html";

   ----------------------
   -- Markdown_to_HTML --
   ----------------------

   function Markdown_To_HTML (Text         : in String;
                              HTML_Enabled : in Boolean)
                              return String
   is
      C_Options : constant int :=
        (if HTML_Enabled then 16#20000# else 0);

      C_Text   :          Strings.chars_ptr := Strings.New_String (Text);
      C_Len    : constant size_t            := size_t (Text'Length);
      C_Result : constant Strings.chars_ptr :=
         C_CMark_Markdown_To_HTML (Text    => C_Text,
                                   Len     => C_Len,
                                   Options => C_Options);
   begin
      Strings.Free (C_Text);

      return Strings.Value (C_Result);
   end Markdown_To_HTML;

end CMark;
