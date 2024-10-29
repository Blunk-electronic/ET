------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            SYMBOL PORTS                                  --
--                                                                          --
--                              S p e c                                     --
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

with ada.strings.bounded; 				use ada.strings.bounded;

with ada.containers; 					use ada.containers;
with ada.containers.indefinite_ordered_maps;

with et_schematic_shapes_and_text;		use et_schematic_shapes_and_text;
with et_coordinates_2;					use et_coordinates_2;
with et_port_names;
with et_port_direction;					use et_port_direction;


package et_symbol_ports is

	use pac_geometry_2;
	use pac_text_schematic;
	
	
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
	
	
	type type_port_name_visible is (YES, NO);
	port_name_visible_default : constant type_port_name_visible := YES;
	function to_string (visible : in type_port_name_visible) return string;
	function to_port_name_visible (visible : in string) return type_port_name_visible;	

	
	type type_terminal_name_visible is (YES, NO);
	terminal_name_visible_default : constant type_terminal_name_visible := YES;
	function to_string (visible : in type_terminal_name_visible) return string;	
	function to_terminal_name_visible (visible : in string) return type_terminal_name_visible;

	
	
	
	-- A port is basically a line. Its start point is the port position.
	-- At the start point a net will be attached.
	-- The end point points towards the symbol body. Depending on the port
	-- rotation the end tail points:
	--  to the left if rotation is 0 degree. net attached from the right.
	--  to the right if rotation is 180 degree. net attached from the left.
	--  downwards if the rotation is 90 degree. net attached from above.
	--  upwards if the rotation is 270 degree. net attached from below.
	
	port_line_width : constant type_distance_positive := 0.2;		-- relevant for GUI only
	port_circle_line_width : constant type_distance_positive := 0.1; 		-- relevant for GUI only
	port_circle_radius : constant type_distance_positive := 0.8;	-- relevant for GUI only

	-- The distance between port end point and port name:
	port_name_spacing : constant type_distance_positive := 2.0;		-- relevant for GUI only

	-- The distance between the line of a port and the terminal name:
	terminal_name_spacing_line : constant type_distance_positive := 1.0; -- relevant for GUI only

	-- The distance between the start point of a port and the
	-- origin of the terminal name:
	terminal_name_spacing_start : constant type_distance_positive := 1.7; -- relevant for GUI only

	
	type type_port_base is tagged record
		position	: type_vector_model; -- this is the point of connection with a net
		length		: type_port_length := port_length_default; 
		
		--rotation	: et_coordinates_2.type_rotation_model := 0.0; -- CS use type_rotation_model_relative ?
		rotation	: et_coordinates_2.type_rotation_relative := 0.0;
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

	use et_port_names;
	use pac_port_name;
	
	package pac_ports is new indefinite_ordered_maps (
		key_type		=> pac_port_name.bounded_string, -- CLOCK, CE, VDD, GND
		element_type	=> type_port);

	
	
	
end et_symbol_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
