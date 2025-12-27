------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            TEXT CONTENT                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;

with et_logging;				use et_logging;


package et_text_content is

	
	
	-- A text may have up to 200 characters which seems sufficient for now.
	text_length_max : constant natural := 200;
	package pac_text_content is new generic_bounded_length (text_length_max);

	function to_string (text_content : in pac_text_content.bounded_string) return string;
	function to_content (content : in string) return pac_text_content.bounded_string;

	empty_text_content : constant pac_text_content.bounded_string :=
		pac_text_content.to_bounded_string ("");
	
	function is_empty (content : in pac_text_content.bounded_string) return boolean;


	-- Clears the given content:
	procedure clear_content (
		content : in out pac_text_content.bounded_string);
	
	
	valid_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ("_-+/: "); 


	-- Tests if the given text contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
	function characters_valid (
		content		: in pac_text_content.bounded_string;
		characters	: in character_set := valid_characters) 
		-- CS log_threshold : in type_log_level)
		return boolean;

	replace_by_default : constant character := '_';

	-- Replaces invalid characters in content by character given in replace_by:
	procedure replace_invalid_characters (
		content		: in out pac_text_content.bounded_string;
		replace_by	: in character := replace_by_default;
		characters	: in character_set := valid_characters);

	
	procedure check_text_content_length (content : in string);
	-- Tests if the content is not longer than allowed.



end et_text_content;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
