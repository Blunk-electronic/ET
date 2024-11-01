------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PACKAGE NAMES                                 --
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
--                                                                          --
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
with ada.characters;			use ada.characters;

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_logging;				use et_logging;
with et_string_processing;		use et_string_processing;



package body et_package_names is


	function to_string (packge : in pac_package_name.bounded_string) return string is
	-- CS: provide a parameter that turns the preamble on/off
	begin
		return pac_package_name.to_string (packge);
	end to_string;


	
	function to_package_name (package_name : in string) return pac_package_name.bounded_string is
	begin
		return pac_package_name.to_bounded_string (package_name);
	end to_package_name;



	
	procedure check_package_name_length (packge : in string) is
	begin
		if packge'length > package_name_length_max then
			log (WARNING, "package name too long. Max. length is" 
				 & positive'image (package_name_length_max) & " !");
		end if;
	end check_package_name_length;



	
	procedure check_package_name_characters (
		packge		: in pac_package_name.bounded_string;
		characters	: in character_set := package_name_characters)
	is
		use pac_package_name;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => packge,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log (WARNING, "package name " & enclose_in_quotes (to_string (packge))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position));
		end if;
	end check_package_name_characters;




	
	function to_string (name : in pac_package_model_file_name.bounded_string) 
		return string is
	begin
		return pac_package_model_file_name.to_string (name);
	end;

	function to_file_name (name : in string) 
		return pac_package_model_file_name.bounded_string is
	begin
		return pac_package_model_file_name.to_bounded_string (name);
	end;


	
	
end et_package_names;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
