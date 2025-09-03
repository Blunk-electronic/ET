------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              SYMBOLS                                     --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_ordered_maps;

with et_schematic_text;					use et_schematic_text;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_schematic_coordinates;			use et_schematic_coordinates;

with et_primitive_objects;				use et_primitive_objects;
with et_text;
with et_fonts;							use et_fonts;
with et_logging;						use et_logging;
with et_symbol_ports;					use et_symbol_ports;
with et_device_appearance;				use et_device_appearance;


package et_symbols is

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
	

	procedure write_text_properies (
	-- Outputs the properties of the given text.
		text 			: in type_text;
		log_threshold	: in type_log_level);

	function content (text : in type_text) return string;
	-- Returns the content of the given text as string.


	package pac_texts is new doubly_linked_lists (type_text);
	


	
	
	
	


	subtype type_line_width is type_distance_positive range 0.1 .. 10.0;
	-- CS rename to type_linewidth_contour ?

	line_width_default : constant type_line_width := 0.2;

	

-- LINES

	type type_symbol_line is new pac_geometry_2.type_line with record
		width	: type_line_width := line_width_default;
	end record;

	
	package pac_symbol_lines is new doubly_linked_lists (type_symbol_line);
	use pac_symbol_lines;
	

	function get_A (
		line : in pac_symbol_lines.cursor)
		return type_vector_model;


	function get_B (
		line : in pac_symbol_lines.cursor)
		return type_vector_model;

	

	
	
-- ARCS
	
	type type_symbol_arc is new pac_geometry_2.type_arc with record
		width	: type_line_width := line_width_default;
	end record;


	procedure set_width (
		arc		: in out type_symbol_arc;
		width	: in type_line_width);

	
	procedure reset_arc (
		arc	: in out type_symbol_arc);
	
	
	package pac_symbol_arcs is new doubly_linked_lists (type_symbol_arc);
	use pac_symbol_arcs;
	
	
	function get_A (
		arc : in pac_symbol_arcs.cursor)
		return type_vector_model;


	function get_B (
		arc : in pac_symbol_arcs.cursor)
		return type_vector_model;


	function get_center (
		arc : in pac_symbol_arcs.cursor)
		return type_vector_model;


	function get_direction (
		arc : in pac_symbol_arcs.cursor)
		return type_direction_of_rotation;

	
	
	type type_circle_filled is (NO, YES);
	
	function to_string (filled : in type_circle_filled) return string;
	
	function to_circle_filled (filled : in string) return type_circle_filled;



	

-- CIRCLES
	
	type type_circle_base is new pac_geometry_2.type_circle with record
		width	: type_line_width := line_width_default;
	end record;

	type type_symbol_circle is new type_circle_base with record
		filled	: type_circle_filled := NO;
	end record;


	procedure set_width (
		circle	: in out type_symbol_circle;
		width	: in type_line_width);

	
	package pac_symbol_circles is new doubly_linked_lists (type_symbol_circle);



	
	-- Shapes are wrapped in a the type_shapes:
	type type_shapes is record
		lines		: pac_symbol_lines.list		:= pac_symbol_lines.empty_list;
		arcs 		: pac_symbol_arcs.list		:= pac_symbol_arcs.empty_list;
		circles		: pac_symbol_circles.list	:= pac_symbol_circles.empty_list;
	end record;


	
-- SYMBOLS AND UNITS

	-- In schematics things like resistors, capactors and inductors are called "symbols".
	-- Since they are frequently used we store such things in symbol libraries like bel_primitives.sym.
	-- The symbol name is something very general like "NAND", "Resistor", "Switch"

	-- A device has one or more units. A unit is a subsection of a device.
	-- There are internal units, which exist for the particular device exclusively. 
	-- An internal unit has a symbol and further properties like a swap level.
	-- There are external units, which are used for frequently used symbols like resistors or capacitors.
	-- An external unit is just a reference to a symbol library, the symbol name therein and other properties
	-- like swap level.	
	-- The unit name is something like "I/O Bank 3", "PWR" or "Switch 1" "Switch 2"

	origin_half_size : constant type_distance_positive := 1.0;
	origin_line_width : constant type_distance_positive := 0.05;
	
	type type_symbol_base is tagged record		
		texts : pac_texts.list; -- the collection of texts
	end record;

	
	type type_symbol (appearance : type_appearance) is new type_symbol_base with record
		shapes			: type_shapes; -- the collection of shapes
		ports			: pac_ports.map;
		case appearance is
			when APPEARANCE_PCB =>
				-- Placeholders for device wide texts. To be filled with content when 
				-- a symbol is placed in the schematic:
				name	: type_text_placeholder (meaning => et_device_placeholders.NAME);
				value	: type_text_placeholder (meaning => et_device_placeholders.VALUE);
				purpose : type_text_placeholder (meaning => et_device_placeholders.PURPOSE);

			when APPEARANCE_VIRTUAL => null;				
		end case;
	end record;



	-- Retrurns x/y-positions the the ports of the given symbol:
	function get_port_positions (
		symbol	: in type_symbol)
		return pac_points.list;

	
		
	-- In the schematic, when a unit is rotated to a certain absolute rotation,
	-- or if the placeholders are to be restored (kind of un-smash),
	-- the default positions of texts and placeholders are required. For this
	-- reason we define here the type type_default_text_positions:
	package pac_text_positions is new doubly_linked_lists (type_vector_model);
	
	type type_default_text_positions (appearance : type_appearance) is record

		-- For texts, we need only their positions (x/y):
		texts : pac_text_positions.list; -- same order as the texts in type_symbol_base

		-- The placeholders are copies of those in the symbol (see type_symbol):
		case appearance is
			when APPEARANCE_PCB =>
				name	: type_text_placeholder (meaning => et_device_placeholders.NAME);
				value	: type_text_placeholder (meaning => et_device_placeholders.VALUE);
				purpose : type_text_placeholder (meaning => et_device_placeholders.PURPOSE);
				
			when APPEARANCE_VIRTUAL => null;
		end case;
	end record;

	

-- FILE NAMES

	symbol_file_name_length_max : constant natural := 500;
	package pac_symbol_model_file is new generic_bounded_length (symbol_file_name_length_max);
	function to_string (name : in pac_symbol_model_file.bounded_string) return string;
	function to_file_name (name : in string) return pac_symbol_model_file.bounded_string;
	


	

	

	

	
	
-- STORAGE
	
	package pac_symbols is new indefinite_ordered_maps (
		key_type		=> pac_symbol_model_file.bounded_string, -- ../libraries/symbols/NAND.sym
		"<"				=> pac_symbol_model_file."<",
		element_type	=> type_symbol);

	use pac_symbols;
	
	symbol_library_file_extension : constant string := "sym";


	
	-- THIS IS THE RIG WIDE LIBRARY OF SYMBOLS:
	
	symbol_library : pac_symbols.map;


	
	
	-- Locates the symbol model in the rig wide symbol library 
	-- by the given file name. Set the cursor accordingly.
	-- If the model has not been found, then the cursor is
	-- set to no_element:
	procedure locate_symbol (
		model_file	: in pac_symbol_model_file.bounded_string;  -- ../libraries/symbols/NAND.sym
		cursor		: in out pac_symbols.cursor);
	
		
	-- Returns true if the given symbol will be part of a real device:
	function is_real (symbol : in pac_symbols.cursor)
		return boolean;


	-- Returns the x/y-positions of the given symbol:
	function get_port_positions (
		symbol	: in pac_symbols.cursor)
		return pac_points.list;

	
	-- When placing, copying, fetching units, placeholders must be
	-- changed according to the rotation of the affected unit. We basically
	-- deal with only those placeholders:
	type type_default_placeholders is record
		name	: type_text_placeholder (meaning => et_device_placeholders.NAME);
		value	: type_text_placeholder (meaning => et_device_placeholders.VALUE);
		purpose	: type_text_placeholder (meaning => et_device_placeholders.PURPOSE);
	end record;

	
	-- This procedure does the actual
	-- rotating of placeholders of a symbol.
	procedure rotate_placeholders (
		phs			: in out type_default_placeholders;
		rotation	: in et_schematic_coordinates.type_rotation_model); -- the rotation of the unit

	
	-- Use this function translates from the rotation of placeholder
	-- described in the symbol model to the rotation of
	-- placeholders of a unit in the schematic.
	-- It translates according to the rotation given by destination:
	function get_default_placeholders (
		symbol_cursor	: in pac_symbols.cursor;
		destination		: in type_object_position) -- x/y/rotation of the unit
		return type_default_placeholders;
	
		
end et_symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
