------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              SYMBOLS                                     --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with cairo;

with et_coordinates;			use et_coordinates;
with et_string_processing;
with et_general;
with et_text;
with et_logging;				use et_logging;


package et_symbols is

	--use pac_geometry_sch;
	use pac_geometry_2;


	
-- TEXT

	text_size_min : constant type_distance_positive := 1.0;
	text_size_max : constant type_distance_positive := 50.0;
	text_size_default : constant type_distance_positive := 1.3;
	
	subtype type_text_line_width is type_distance_positive range 0.0 .. 5.0; -- unit is mm -- CS: minimum of 0.0 reasonable ?
	text_line_width_min : constant type_distance_positive := 0.1;
	text_line_width_max : constant type_distance_positive := 5.0;
	text_line_width_default : constant type_distance_positive := 0.3; 

	
	-- Instantiation of the text package:
	package pac_text_schematic is new et_text.generic_pac_text (
		pac_geometry_2		=> pac_geometry_2,
		pac_polygons		=> pac_polygons, -- never used, but mandatory for instantiation
		size_min			=> text_size_min,
		size_max			=> text_size_max,
		size_default		=> text_size_default,
		line_width_min		=> text_line_width_min,
		line_width_max		=> text_line_width_max,
		line_width_default	=> text_line_width_default
		);

	use pac_text_schematic; 
	
-- CS: if required some day, move to package et_text:
-- 	type type_text_style is (NORMAL, ITALIC, BOLD, ITALIC_BOLD);
-- 	function to_string (style : in type_text_style) return string;
-- 	function to_text_style (style : in string) return type_text_style;
	

	-- Text placeholders have a meaning:
	type type_placeholder_meaning is (
		NAME,	-- for things like R301 or X9
		VALUE,	-- for component values like "200R"
		PURPOSE	-- for the purpose of the component in the design.
		);
	
	placeholder_meaning_default : constant type_placeholder_meaning := NAME;
	
	function to_string (meaning : in type_placeholder_meaning) return string;
	function to_meaning (meaning : in string) return type_placeholder_meaning;


	-- GUI relevant only:
	name_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	value_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_ITALIC,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	purpose_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);
	

	
	
	-- These are basic properties a text has got:
	type type_text_basic is new type_text with record
		-- CS font : type_font; ?
        rotation	: et_text.type_rotation_documentation := et_text.type_rotation_documentation'first;
	end record;
	
	-- This is a placeholder for a name, value or purpose.
	-- It does not have content yet, but a meaning.
	-- The position is just x/y relative to the symbol origin.
	type type_text_placeholder (meaning : type_placeholder_meaning) is new type_text_basic with record
		position : type_point;
	end record;

	procedure write_placeholder_properties (
	-- Writes the properties of the given placeholder.
		placeholder		: in type_text_placeholder;
		log_threshold	: in type_log_level);

	
	-- This is a real text with content (used for things like "counter" or "decoder"
	type type_text is new type_text_basic with record
		position	: type_point;		
        content		: et_text.pac_text_content.bounded_string;
	end record;

	-- GUI relevant only:
	text_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);
	

	procedure write_text_properies (
	-- Outputs the properties of the given text.
		text 			: in type_text;
		log_threshold	: in type_log_level);

	function content (text : in type_text) return string;
	-- Returns the content of the given text as string.


	package pac_texts is new doubly_linked_lists (type_text);
	



	
	
-- TERMINALS

	keyword_terminal				: constant string := "terminal";
	keyword_terminal_name_visible	: constant string := "terminal_name_visible";
	keyword_terminal_name_size		: constant string := "terminal_name_size";

	


	
	
-- PORTS
	keyword_port					: constant string := "port";
	keyword_port_name_visible		: constant string := "port_name_visible";
	keyword_port_name_size			: constant string := "port_name_size";
	keyword_length					: constant string := "length";
	keyword_level					: constant string := "level";	
	keyword_sensitivity_edge		: constant string := "sensitivity_edge";
	keyword_sensitivity_level		: constant string := "sensitivity_level";
	keyword_inverted				: constant string := "inverted";
	keyword_weakness				: constant string := "weakness";
	keyword_tristate				: constant string := "tristate";	
	keyword_output_inverted			: constant string := "output_inverted";
	keyword_output_weakness			: constant string := "output_weakness";
	keyword_output_tristate			: constant string := "output_tristate";
	keyword_input_sensitivity_edge	: constant string := "input_sensitivity_edge";
	keyword_input_sensitivity_level	: constant string := "input_sensitivity_level";
	
	-- A port is something where a net can be attached to.
	-- The name of a port represents the function of the port like (A14 or RST_N)
	subtype type_port_length is type_distance_positive range 0.0 .. 20.0; -- unit is millimeters.
	port_length_default : constant type_port_length := 2.5;
	
	-- The port has an electrical direction:

	keyword_direction : constant string := "direction";	
	
	type type_port_direction is (
		PASSIVE,		-- almost all passive components like resistors, capacitors, .. have such ports

		INPUT_ANALOG,	-- signal input analog
		INPUT_DIGITAL,	-- signal input digital

		OUTPUT_ANALOG,	-- signal output analog		
		OUTPUT_DIGITAL,	-- signal outputs

		BIDIR_DIGITAL,	-- bidirectional ports
		-- CS BIDIR_ANALOG, ??

		POWER_OUT,		-- a power source like power symbol (VCC, GND, ..)
		POWER_IN,		-- a power sink like power ports of ICs

		NOT_CONNECTED	-- advised by manufacturer to be left unconnected
		);

	port_direction_default : constant type_port_direction := OUTPUT_ANALOG; 
	-- CS: should be the one with the most severe implications.
	
	function to_string (direction : in type_port_direction) return string;
	function to_port_direction (direction : in string) return type_port_direction;
	
	type type_port_name_visible is (YES, NO);
	port_name_visible_default : constant type_port_name_visible := YES;
	function to_string (visible : in type_port_name_visible) return string;
	function to_port_name_visible (visible : in string) return type_port_name_visible;	

	
	type type_terminal_name_visible is (YES, NO);
	terminal_name_visible_default : constant type_terminal_name_visible := YES;
	function to_string (visible : in type_terminal_name_visible) return string;	
	function to_terminal_name_visible (visible : in string) return type_terminal_name_visible;

	
 	port_name_length_max : constant natural := 100;
	package pac_port_name is new generic_bounded_length (port_name_length_max);
	use pac_port_name;

	function to_string (port : in pac_port_name.bounded_string) return string;
	function to_port_name (name : in string) return pac_port_name.bounded_string;
	
	
	-- line width
	keyword_line_width : constant string := "line_width"; -- NOTE: do not confuse with text line width !
	subtype type_line_width is type_distance_positive range 0.1 .. 10.0;
	line_width_default : constant type_line_width := 0.15;
	
	-- A port is basically a line. Its start point is the port position.
	-- At the start point a net will be attached.
	-- The end point points towards the symbol body. Depending on the port
	-- rotation the end tail points:
	--  to the left if rotation is 0 degree. net attached from the right.
	--  to the right if rotation is 180 degree. net attached from the left.
	--  downwards if the rotation is 90 degree. net attached from above.
	--  upwards if the rotation is 270 degree. net attached from below.
	
	port_line_width : constant type_line_width := 0.2;				-- relevant for GUI only
	port_circle_line_width : constant type_line_width := 0.1; 		-- relevant for GUI only
	port_circle_radius : constant type_distance_positive := 0.8;	-- relevant for GUI only

	-- The distance between port end point and port name:
	port_name_spacing : constant type_distance_positive := 2.0;		-- relevant for GUI only

	-- The distance between the line of a port and the terminal name:
	terminal_name_spacing_line : constant type_distance_positive := 1.0; -- relevant for GUI only

	-- The distance between the start point of a port and the
	-- origin of the terminal name:
	terminal_name_spacing_start : constant type_distance_positive := 1.7; -- relevant for GUI only

	
	type type_port_base is tagged record
		position	: type_point; -- this is the point of connection with a net
		length		: type_port_length := port_length_default; 
		
		--rotation	: et_coordinates.type_rotation := 0.0; -- CS use type_rotation_relative ?
		rotation	: et_coordinates.type_rotation_relative := 0.0;
		--  90.0 -- to be connected with a net from above,
		-- -90.0 -- from below,
		-- 180.0 -- from the left,
		--   0.0 -- from the right
		
		port_name_visible		: type_port_name_visible := port_name_visible_default;
		port_name_size			: type_text_size := text_size_default;
		
		terminal_name_visible	: type_terminal_name_visible := terminal_name_visible_default;
		terminal_name_size		: type_text_size := text_size_default;
		-- CS: port swap level ? -> would require a derived new type
	end record;

	-- Sensitity of inputs:
	type type_sensitivity_edge is (
		NONE, 		-- passive and analog
		RISING,		-- digital
		FALLING,	-- digital
		ANY			-- digtial
		);
	
	sensitivity_edge_default : constant type_sensitivity_edge := NONE;
	function to_string (sensitivity : in type_sensitivity_edge) return string;
	function to_sensitivity_edge (sensitivity : in string) return type_sensitivity_edge;

	type type_sensitivity_level is (NONE, LOW, HIGH); -- CS NONE required ?
	sensitivity_level_default : constant type_sensitivity_level := HIGH; -- CS good idea ?
	function to_string (sensitivity : in type_sensitivity_level) return string;
	function to_sensitivity_level (sensitivity : in string) return type_sensitivity_level;
	
	type type_output_inverted is (NO, YES);
	output_inverted_default : constant type_output_inverted := NO;
	function to_string (inverted : in type_output_inverted) return string;
	function to_output_inverted (inverted : in string) return type_output_inverted;

	type type_output_weakness is (
		NONE, -- push-pull
		WEAK0, WEAK1, -- requires external pull-down/up resistor
		PULL0, PULL1  -- internal pull-down/up resistor
		);
	output_weakness_default : constant type_output_weakness := NONE;
	function to_string (weakness : in type_output_weakness) return string;
	function to_output_weakness (weakness : in string) return type_output_weakness;

	type type_output_tristate is (NO, YES);
	output_tristate_default : constant type_output_tristate := NO;
	function to_string (tristate : in type_output_tristate) return string;
	function to_output_tristate (tristate : in string) return type_output_tristate;

	
	type type_power_level is (LEVEL_ZERO, LEVEL_POSITIVE, LEVEL_NEGATIVE);
	-- The prefix "LEVEL_" is a workaround because GNAT regards "POSITIVE" as keyword.
	-- CAUTION: Adapt functions to_string and to_power_level when changing anything here !
	
	port_power_level_default : constant type_power_level := LEVEL_ZERO;

	function to_string (level : in type_power_level) return string;
	-- Converts the power level (like LEVEL_POSITIVE) to a string (like positive).
	-- The prefix LEVEL_ is removed.
	
	function to_power_level (level : in string) return type_power_level;	
	-- Converts the power level (like positive) to power level (like LEVEL_POSITIVE).
	-- The prefix LEVEL_ is prepended.


	
	type type_port (direction : type_port_direction) is new type_port_base with record 
		case direction is
			when INPUT_DIGITAL =>
				sensitivity_edge		: type_sensitivity_edge;
				sensitivity_level		: type_sensitivity_level;

			when OUTPUT_ANALOG =>
				output_analog_tristate	: type_output_tristate;
				output_analog_weakness	: type_output_weakness;
				
			when OUTPUT_DIGITAL =>
				output_digital_inverted	: type_output_inverted;
				output_digital_tristate	: type_output_tristate;
				output_digital_weakness	: type_output_weakness;
				
			when BIDIR_DIGITAL =>
				output_inverted			: type_output_inverted;
				output_tristate			: type_output_tristate;				
				output_weakness			: type_output_weakness;
				input_sensitivity_edge	: type_sensitivity_edge;
				input_sensitivity_level	: type_sensitivity_level;
				
			when POWER_OUT | POWER_IN =>
				level	: type_power_level;
				
			when others => null;
		end case;
	end record;
	
	-- Ports of a symbol are collected in a map. A map because a port with a certain name
	-- like GND may exist only once in the symbol. Te symbol is an abstraction of a
	-- function block within a device. It does not matter how many GND terminals exist
	-- at the package (footprint):
	package pac_ports is new indefinite_ordered_maps (
		key_type		=> pac_port_name.bounded_string, -- CLOCK, CE, VDD, GND
		element_type	=> type_port);

	
	

	

	
	
	
-- APPEARANCE	
	type type_appearance is (
		VIRTUAL,	-- a device that exists in the schematic only (like power symbols)
		PCB			-- a device that exists in both schematic and soldered on a pcb
		-- CS: cable 
		-- CS: wire
		-- ...
		);

	function to_string (
		appearance	: in type_appearance;
		verbose		: in boolean := false)
		return string;
	-- Returns the given device appearance as string.

	function to_appearance (appearance : in string) return type_appearance;
	


	




	-- lines
	type type_line is new pac_geometry_2.type_line with record
		width		: type_line_width := line_width_default;
	end record;
	package pac_lines is new doubly_linked_lists (type_line);

	-- Arcs
	type type_arc is new pac_geometry_2.type_arc with record
		width		: type_line_width := line_width_default;
	end record;
	package pac_arcs is new doubly_linked_lists (type_arc);

	type type_circle_filled is (NO, YES);
	function to_string (filled : in type_circle_filled) return string;
	function to_circle_filled (filled : in string) return type_circle_filled;
	
	-- Circles
	type type_circle_base is new pac_geometry_2.type_circle with record
		width		: type_line_width := line_width_default;
	end record;

	type type_circle is new type_circle_base with record
		filled		: type_circle_filled := NO;
	end record;
	package pac_circles is new doubly_linked_lists (type_circle);

	-- Shapes are wrapped in a the type_shapes:
	type type_shapes is record
		lines		: pac_lines.list 	:= pac_lines.empty_list;
		arcs 		: pac_arcs.list		:= pac_arcs.empty_list;
		circles		: pac_circles.list	:= pac_circles.empty_list;
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
			when PCB =>
				-- Placeholders for device wide texts. To be filled with content when 
				-- a symbol is placed in the schematic:
				name	: type_text_placeholder (meaning => et_symbols.NAME);
				value	: type_text_placeholder (meaning => et_symbols.VALUE);
				purpose : type_text_placeholder (meaning => et_symbols.PURPOSE);

			when VIRTUAL => null;				
		end case;
	end record;


	-- In the schematic, when a unit is rotated to a certain absolute rotation,
	-- or if the placeholders are to be restored (kind of un-smash),
	-- the default positions of texts and placeholders are required. For this
	-- reason we define here the type type_default_text_positions:
	package pac_text_positions is new doubly_linked_lists (type_point);
	
	type type_default_text_positions (appearance : type_appearance) is record

		-- For texts, we need only their positions (x/y):
		texts : pac_text_positions.list; -- same order as the texts in type_symbol_base

		-- The placeholders are copies of those in the symbol (see type_symbol):
		case appearance is
			when PCB =>
				name	: type_text_placeholder (meaning => et_symbols.NAME);
				value	: type_text_placeholder (meaning => et_symbols.VALUE);
				purpose : type_text_placeholder (meaning => et_symbols.PURPOSE);
				
			when VIRTUAL => null;
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

	-- HERE RIG WIDE EXTERNAL SYMBOLS ARE KEPT:	
	symbols : pac_symbols.map;

	-- NOTE: Devices can use internal or external symbols. An internal symbol
	-- is modelled inside the device, is fixed to the that device
	-- and can be used by that device exclusively.
	-- For example after importing a KiCad project there will only be internal
	-- symbols.
	--  External symbols provide much more flexibilty as they can be used by many
	-- devices. There is no fixed connection between device and symbol.
	
	function locate (symbol : in pac_symbol_model_file.bounded_string) -- ../libraries/symbols/NAND.sym
		return pac_symbols.cursor;

	-- Returns true if the given symbol will be part of a real device:
	function is_real (symbol : in pac_symbols.cursor)
		return boolean;
	
	
	-- When placing, copying, invoking units their placeholders must be
	-- changed according to the rotation of the affected unit. We basically
	-- deal with only those placeholders:
	type type_rotated_placeholders is record
		name	: type_text_placeholder (meaning => et_symbols.NAME);
		value	: type_text_placeholder (meaning => et_symbols.VALUE);
		purpose	: type_text_placeholder (meaning => et_symbols.PURPOSE);
	end record;

	
	-- This procedure is called by function et_symbols.rotate_placeholders and
	-- by the function et_devices.rotate_placeholders. It does the actual
	-- rotating of placeholders of a symbol.
	procedure rotate (
		phs			: in out type_rotated_placeholders;
		rotation	: in et_coordinates.type_rotation); -- the rotation of the unit

	
	-- Use this function to adopt placeholder position and rotation 
	-- of a external symbol. Exter
	-- Rotates the positions of placeholders and their rotation about
	-- their own origin according to rotation given by destination:
	function rotate_placeholders (
		symbol_cursor	: in pac_symbols.cursor;
		destination		: in et_coordinates.type_position) -- x/y/rotation of the unit
		return type_rotated_placeholders;
	
		
end et_symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
