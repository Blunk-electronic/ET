------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        DEVICE PLACEHOLDERS                               --
--                                                                          --
--                              S p e c                                     --
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
--   to do:

with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.maps;			use ada.strings.maps;

with et_fonts;


package et_device_placeholders is



	-- Text placeholders have a meaning:
	type type_placeholder_meaning is (
		NAME,	-- for things like R301 or X9
		VALUE,	-- for component values like "200R"
		PURPOSE	-- for the purpose of the component in the design.
		-- CS partcode ?
		);
	-- CS use prefix
	

	placeholder_meaning_default : constant type_placeholder_meaning := NAME;
	
	
	function to_string (
		text_meaning : in type_placeholder_meaning) 
		return string;

	
	function to_meaning (
		text_meaning : in string) 
		return type_placeholder_meaning;
	



	-- This enumeration type shall be used in order to
	-- specify how a placeholder is tied to a package.
	type type_anchor_mode is (
	
		-- The placeholder position is relative to the 
		-- origin of the complex parent object.
		-- If the object is moved or rotated, then the 
		-- placeholder moves along:
		RELATIVE,
		
		-- The placeholder position is absolute, independend
		-- of the parent object position. So the placeholder is completely
		-- disconnected from the object:
		ABSOLUTE);

		
	anchor_mode_default : constant type_anchor_mode := RELATIVE;
	


	function to_string (
		mode : in type_anchor_mode)
		return string;


	function to_anchor_mode (
		mode : in string)
		return type_anchor_mode;
	
	
	
	-- CS ?
	-- type type_rotation_mode is (
	-- 	MODE_ROTATE_WITH_PACKAGE
	-- 	...);
	
	
end et_device_placeholders;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
