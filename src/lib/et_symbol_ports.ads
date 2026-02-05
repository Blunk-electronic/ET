------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            SYMBOL PORTS                                  --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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
--
-- To Do:
-- - rename this package to et_symbol_port (singular)
--

with ada.containers; 					use ada.containers;
with ada.containers.indefinite_ordered_maps;

with et_net_port_length;				use et_net_port_length;
with et_net_linewidth;					use et_net_linewidth;
with et_schematic_text;					use et_schematic_text;
with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_port_names;
with et_port_direction;					use et_port_direction;
with et_port_visibility;				use et_port_visibility;
with et_port_sensitivity;				use et_port_sensitivity;
with et_port_strength;					use et_port_strength;
with et_logic;							use et_logic;
with et_power_sources;					use et_power_sources;


package et_symbol_ports is

	use pac_geometry_2;
	use pac_text_schematic;
	

	port_length_default : constant type_port_length := 2.5;
	
	
	-- A port is basically a line with a linewidth equal to those
	-- of net segments.
	-- Its start point is the port position.
	-- At the start point a net will be attached.
	-- The end point points towards the symbol body. Depending on the port
	-- rotation the end tail points:
	--  to the left if rotation is 0 degree. net attached from the right.
	--  to the right if rotation is 180 degree. net attached from the left.
	--  downwards if the rotation is 90 degree. net attached from above.
	--  upwards if the rotation is 270 degree. net attached from below.
	
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
		
		--rotation	: et_schematic_coordinates.type_rotation_model := 0.0; -- CS use type_rotation_model_relative ?
		rotation	: type_rotation_relative := 0.0;
		--  90.0 -- to be connected with a net from above,
		-- -90.0 -- from below,
		-- 180.0 -- from the left,
		--   0.0 -- from the right
		
		port_name_visible		: type_port_name_visible := port_name_visible_default;

		-- CS: This selector is probably not required
		-- if we use a hardcoded text size for the name:
		port_name_size			: type_text_size := text_size_default;
		
		terminal_name_visible	: type_terminal_name_visible := terminal_name_visible_default;
		terminal_name_size		: type_text_size := text_size_default;
		-- CS: port swap level ? -> would require a derived new type
	end record;

	



	
	type type_symbol_port (direction : type_port_direction) 
	is new type_port_base with record 
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

	
	package pac_symbol_ports is new indefinite_ordered_maps (
		key_type		=> pac_port_name.bounded_string, -- CLOCK, CE, VDD, GND
		element_type	=> type_symbol_port);

	use pac_symbol_ports;



	function get_port_name (
		port_cursor : in pac_symbol_ports.cursor)
		return pac_port_name.bounded_string;

	

	-- Returns the total number of ports in the given list:
	function get_count (
		ports : in pac_symbol_ports.map)
		return natural;
	

	-- Returns the x/y-position of the given port:
	function get_position (							  
		port	: in pac_symbol_ports.cursor)
		return type_vector_model;


	-- Deletes the first port in the given list
	-- that sits at the given position. If a port
	-- has been found (and deleted) then the flag
	-- "deleted" is set and the name of the port output:
	procedure delete_port (
		ports		: in out pac_symbol_ports.map;
		position	: in type_vector_model;
		deleted		: out boolean;
		port_name	: out pac_port_name.bounded_string);
	
	
	-- Moves the given ports by given offset.
	procedure move_ports (
		ports	: in out pac_symbol_ports.map; -- the portlist
		offset	: in type_object_position); -- the offset (only x/y matters)
	

	-- Rotates the given ports by given 
	-- angle about the origin of the symbol:
	procedure rotate_ports (
		ports	: in out pac_symbol_ports.map; -- the portlist
		angle	: in type_rotation_model); -- 90



	-- CS procedure that outputs the properties of a port
	
end et_symbol_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
