------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        GENERAL READ AND WRITE                            --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.exceptions;

package body et_general_rw is
	

-- SECTIONS AND INDENTATION
	function write_top_level_reached return string is begin return "top level reached"; end;
	function write_enter_section return string is begin return "entering section "; end;
	function write_return_to_section return string is begin return "returning to section "; end;

	function write_missing_begin_end return string is begin 
		return "missing section begin or section end after section name !"; 
	end;

	function write_section_stack_not_empty return string is begin
		return "section stack not empty !"; end;
	
	procedure invalid_section is begin
		log (ERROR, "invalid section name !", console => true);
		raise constraint_error;
	end;
	


	procedure invalid_arc is begin
		log (ERROR, "Start and end point of arc have differing distance to center !",
			 console => true);
		raise constraint_error;
	end invalid_arc;
	


	
end et_general_rw;
