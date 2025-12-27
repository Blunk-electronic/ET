------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            TEXT CONTENT                                  --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.characters.handling;	use ada.characters.handling;

with et_string_processing;		use et_string_processing;
-- with et_exceptions;				use et_exceptions;


package body et_text_content is



	function to_string (text_content : in pac_text_content.bounded_string) return string is begin
		return pac_text_content.to_string (text_content);
	end to_string;

	
	
	function to_content (content : in string) return pac_text_content.bounded_string is begin
		return pac_text_content.to_bounded_string (content);
	end to_content;



	
	
	function is_empty (content : in pac_text_content.bounded_string) return boolean is begin
		if pac_text_content.length (content) > 0 then -- contains something -> not empty
			return false;
		else
			return true; -- contains nothing -> is empty
		end if;
	end is_empty;
	


	
	procedure clear_content (
		content : in out pac_text_content.bounded_string)
	is begin
		content := empty_text_content;
	end;

	

	
	
	function characters_valid (
		content		: in pac_text_content.bounded_string;
		characters	: in character_set := valid_characters) 		
		return boolean 
	is
		use pac_text_content;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> content,
			set 	=> characters,
			test 	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "Text " & enclose_in_quotes (to_string (content))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position)
				 & " !"
				);
			return false;
		else
			return true;
		end if;
	end characters_valid;



	
	
	procedure replace_invalid_characters (
		content		: in out pac_text_content.bounded_string;
		replace_by	: in character := replace_by_default;
		characters	: in character_set := valid_characters)
	is
		use pac_text_content;
		invalid_character_position : natural := 0;

		l_max : natural;
	begin
		if characters_valid (content) then
			null;
		else
			log (WARNING, "Replacing invalid characters in text " 
				& enclose_in_quotes (to_string (content))
				& " by " & enclose_in_quotes (replace_by) & " !");

			-- To prevent an infintive loop, we test for invalid characters
			-- no more often than the length of the given content:
			l_max := length (content);
			
			for p in 0 .. l_max loop
				
				invalid_character_position := index (
					source	=> content,
					set 	=> characters,
					test 	=> outside);

				-- If there is an invalid character, replace it at the detected
				-- position. Eventually there are no more invalid characters
				-- and the loop ends prematurely.
				if invalid_character_position > 0 then
					replace_element (content, invalid_character_position, replace_by);
				else
					exit;
				end if;
				
			end loop;
		end if;
	end replace_invalid_characters;


	

	
	procedure check_text_content_length (content : in string) is
	begin
		if content'length > text_length_max then
			log (ERROR, "max. number of characters for a text field is" 
				 & positive'image (text_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_text_content_length;




end et_text_content;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
