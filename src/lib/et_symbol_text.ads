------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      SCHEMATIC SYMBOL TEXT                               --
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
-- DESCRIPTION:
--
--  This package is about so called "symbols". A symbol is an abstraction
--  of an electrical component like a resistor, capactor, inductor or
--  a subset of an integrated circuit.
--
--   history of changes:
--


with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with et_schematic_text;					use et_schematic_text;
with et_schematic_geometry;				use et_schematic_geometry;

with et_logging;						use et_logging;

with et_text;
with et_fonts;							use et_fonts;


package et_symbol_text is

	use pac_geometry_2;
	use pac_text_schematic;


	
	-- This is a real text with content (used for things like "counter" or "decoder"
	type type_text is new type_text_basic with record
		position	: type_vector_model;		
        content		: et_text.pac_text_content.bounded_string;
	end record;

	
	-- GUI relevant only:
	text_font : constant type_font := 
		to_font (FAMILY_MONOSPACE, SLANT_NORMAL, WEIGHT_NORMAL);
	

	
	-- Outputs the properties of the given text.
	procedure write_text_properies (
		text 			: in type_text;
		log_threshold	: in type_log_level);


	
	-- Returns the content of the given text as string.
	function content (text : in type_text) return string;


	package pac_texts is new doubly_linked_lists (type_text);
	
	

end et_symbol_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
