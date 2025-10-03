------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   DEVICE PLACEHOLDERS IN SYMBOLS                         --
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

with et_text;					use et_text;
with et_schematic_geometry;		use et_schematic_geometry;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_schematic_text;			use et_schematic_text;
with et_logging;				use et_logging;

with cairo;

package et_device_placeholders.symbols is

	use et_fonts;
	use pac_geometry_2;
	use pac_text_schematic;
	
	-- This is a placeholder for a name, value or purpose.
	-- It does not have content yet, but a meaning.
	-- The position is just x/y relative to the symbol origin.
	type type_text_placeholder (
		meaning : type_placeholder_meaning) 
	is new type_text_basic with record
		position : type_vector_model;
	end record;


	-- Returns true if the given placeholder is in
	-- the given catch zone:
	function in_catch_zone (
		placeholder	: in type_text_placeholder;
		zone		: in type_catch_zone)
		return boolean;

	

	type type_default_placeholders is record
		name	: type_text_placeholder (meaning => et_device_placeholders.NAME);
		value	: type_text_placeholder (meaning => et_device_placeholders.VALUE);
		purpose	: type_text_placeholder (meaning => et_device_placeholders.PURPOSE);
	end record;
	


	-- Clears the proposed-flag and the selected-flag of the placeholders:	
	procedure reset_status (
		placeholders : in out type_default_placeholders);

	
	
	-- Writes the properties of the given placeholder.
	procedure write_placeholder_properties (
		placeholder		: in type_text_placeholder;
		log_threshold	: in type_log_level);




	

	-- GUI relevant only:
	name_font : constant type_font := (
		family	=> to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	value_font : constant type_font := (
		family	=> to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_ITALIC,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	purpose_font : constant type_font := (
		family	=> to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);
	

	
end et_device_placeholders.symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
