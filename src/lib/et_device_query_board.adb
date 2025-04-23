------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICE QUERY OPERATIONS IN BOARD                       --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_device_appearance;				use et_device_appearance;
with et_device_purpose;					use et_device_purpose;
with et_mirroring;						use et_mirroring;
with et_schematic_ops;
with et_contour_to_polygon;
with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_package_names;					use et_package_names;
with et_device_value;					use et_device_value;
with et_device_name;					use et_device_name;


package body et_device_query_board is

	use et_symbols;
	use pac_geometry_2;

	
	



	


	procedure terminal_not_found (
		terminal_name : in et_terminals.pac_terminal_name.bounded_string) 
	is 
		use et_terminals;
	begin
		log (ERROR,	"terminal " & enclose_in_quotes (to_string (terminal_name)) & " not found !",
			 console => true);
		raise constraint_error;
	end terminal_not_found;

	

	function get_terminal_position (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor; -- IC45
		terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position_fine
	is
		-- This is the position of the package as it is in the layout:
		package_position : et_pcb_coordinates_2.type_package_position; -- incl. angle and face

		use pac_geometry_brd;
		terminal_position : type_vector; -- x/y
		terminal_rotation : type_angle;
		terminal_position_face : type_face := TOP; -- top/bottom

		model : pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
		package_model_cursor : pac_package_models.cursor;

		use pac_terminals;
		-- This cursor points to the terminal in the package model:
		terminal_cursor : pac_terminals.cursor;
		
		terminal_technology : type_assembly_technology;
		
	begin
		-- Get the package model of the given device:
		model := get_package_model (device_cursor);

		-- Get the position of the package as it is in the layout:
		package_position := pac_devices_sch.element (device_cursor).position;
		
		-- Set the cursor to package model:
		package_model_cursor := get_package_model (model);

		-- Locate the desired terminal in the package model:
		terminal_cursor := get_terminal (package_model_cursor, terminal_name);
		if terminal_cursor = pac_terminals.no_element then
			terminal_not_found (terminal_name);
		end if;

		-- Get the assembly technology of the terminal (SMT or THT):
		terminal_technology := element (terminal_cursor).technology;

		-- Get x/y of the terminal as given by the package model.
		-- This position is relative to the origin of the package model:
		terminal_position := to_vector (pac_terminals.element (terminal_cursor).position.place);
		
		-- Get the rotation of the terminal (about its center) as given by the package model:
		terminal_rotation := to_angle (pac_terminals.element (terminal_cursor).position.rotation);

		-- Add to the terminal rotation the rotation of the package:
		terminal_rotation := terminal_rotation + to_angle (get_rotation (package_position));

		
		-- In the board: If the package has been flipped by the operator
		-- then the terminal must be flipped also. In case of a THT terminal,
		-- flipping the terminal has no effect, since for THT there is no "face"-property:
		case get_face (device_cursor) is
			when BOTTOM =>

				case terminal_technology is
					when SMT =>
						if element (terminal_cursor).face = TOP then
							terminal_position_face := BOTTOM;
						else
							terminal_position_face := TOP;
						end if;

					when THT => 
						terminal_position_face := BOTTOM;
				end case;

				
				-- mirror terminal position alog Y axis (swap right x with left x)
				mirror (terminal_position, MIRROR_ALONG_Y_AXIS);

				-- Rotate the terminal position (x/y) by the rotation of the package:
				rotate_by (terminal_position, - terminal_rotation);

				
			when TOP =>

				case terminal_technology is
					when SMT =>
						terminal_position_face := element (terminal_cursor).face;

					when THT => 
						terminal_position_face := TOP;
				end case;

				
				-- Rotate the terminal position (x/y) by the rotation of the package:
				rotate_by (terminal_position, terminal_rotation);
		end case;


		-- Move the terminal position by the position of the package:
		move_by (terminal_position, to_offset (package_position.place));

		return (
			technology	=> terminal_technology,
			place		=> terminal_position,
			rotation	=> terminal_rotation,	   
			face		=> terminal_position_face);
		
	end get_terminal_position;



	




	function get_unconnected_terminals (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return pac_terminals.map
	is
		use pac_generic_modules;
		use et_nets;
		use pac_terminals;
		use et_schematic_ops;

		-- Get all terminals of the given device (according to its package variant).
		-- Later the connected terminals will be removed from this list:
		all_terminals : pac_terminals.map := get_all_terminals (device_cursor);

		-- Here we will store the terminals of the given device which are
		-- connected with nets:
		connected_terminals : pac_terminal_names.list;

		
		procedure query_net (net_cursor : in pac_nets.cursor) is
			-- Get the ports of all devices connected with the given net.
			-- Since this query is about the default assembly variant,
			-- we do not pass a specific assembly variant here.
			ports : constant type_ports := get_ports (net_cursor);

			use pac_device_ports;

			
			procedure query_device_port (d : in pac_device_ports.cursor) is

				port : type_device_port renames element (d);
				-- Now port contains the device name, unit name and port name.
				
				-- Get the cursor to the device in the schematic:
				device_cursor : constant pac_devices_sch.cursor := 
					locate_device (module_cursor, port.device_name);

				-- Get the cursor to the physical terminal (in the package model)
				-- that is linked with the port:
				terminal_cursor : constant pac_terminals.cursor := 
					get_terminal (device_cursor, port.unit_name, port.port_name);

				-- Get the terminal name (like 3 or H5):
				terminal_name : constant pac_terminal_name.bounded_string := 
					key (terminal_cursor);
				
			begin
				--put_line ("dev " & to_string (key (net_cursor)));
				if key (device_cursor) = key (get_unconnected_terminals.device_cursor) then
				
				-- Store the terminal name in list connected_terminals:
					connected_terminals.append (terminal_name);
				end if;
			end query_device_port;

			
		begin
			--put_line ("net " & to_string (key (net_cursor)));
			
			-- In variable "ports" we are interested in selector "devices" exclusively.
			-- Submodule ports and netchangers are just virtual devices
			-- that connect two conductor tracks. They can therefore be ignored:
			ports.devices.iterate (query_device_port'access);
		end query_net;

		
	begin
		--put_line ("device " & to_string (key (device_cursor)));
		--put_line ("all " & count_type'image (all_terminals.length));
		
		element (module_cursor).nets.iterate (query_net'access);

		--put_line ("connected " & count_type'image (connected_terminals.length));
		
		-- Remove the connected_terminals from all_terminals
		-- so that only the unconneced terminals are left:
		remove_terminals (all_terminals, connected_terminals);

		return all_terminals;
	end get_unconnected_terminals;


	

	function to_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive)
		return type_terminal_polygon
	is
		use pac_polygons;
		exists : boolean := false;
		result : type_polygon; -- to be returned

		use pac_contours;
		use pac_terminals;
		
		-- Get the actual terminal as described in the package model:
		terminal : constant et_terminals.type_terminal := element (terminal_cursor);
		
		-- Get the terminal name (like 3 or H5):
		terminal_name : constant pac_terminal_name.bounded_string := key (terminal_cursor);
		
		-- Get the terminal position (incl. rotation and face):
		terminal_position : constant type_terminal_position_fine := 
			get_terminal_position (module_cursor, device_cursor, terminal_name);

		-- The displacement required to move the contour to 
		-- its final position:
		terminal_displacement : constant type_distance_relative := 
			to_distance_relative (terminal_position.place);

		
		-- intermediate storage place of a contour:
		contour : type_contour;


		-- Converts the contour to a polygon:
		procedure make_polygon is 
			use et_contour_to_polygon;
		begin
			exists := true;
			
			result := to_polygon (
				contour		=> contour,
				tolerance	=> tolerance,
				mode		=> EXPAND, -- CS ?
				debug		=> false);
		end make_polygon;


		-- Mirrors the contour (if terminal is flipped to bottom side) and
		-- rotates the contour:
		procedure mirror_and_rotate is begin
			if terminal_position.face = BOTTOM then
				mirror (contour, MIRROR_ALONG_Y_AXIS);

				-- if on bottom side: rotate CW
				rotate_by (contour, - to_rotation (terminal_position.rotation));
			else
				-- if on top side: rotate CCW
				rotate_by (contour, + to_rotation (terminal_position.rotation));
			end if;
		end mirror_and_rotate;


		-- Moves the contour to the final position and converts it to a polygon.
		-- Optionally, if required by the caller, offsets the polygon edges
		-- by the width of the inner signal layer:
		procedure finalize (do_offset : in boolean := false) is
			use et_board_shapes_and_text.pac_polygon_offsetting;
		begin
			move_by (contour, terminal_displacement);
			make_polygon;
			if do_offset then
				offset_polygon (result, type_float (terminal.width_inner_layers));
			end if;
		end finalize;
				
		
	begin -- to_polygon

		case terminal.technology is
			when THT => 
				case layer_category is
					when INNER =>								
						case terminal.tht_hole is
							when DRILLED =>
								contour := get_inner_contour (terminal, terminal_position.place);
								make_polygon;										
								
							when MILLED =>
								contour := terminal.millings;
								mirror_and_rotate;										
								finalize (do_offset => true);
								
						end case;
					
					when OUTER_TOP =>
						if terminal_position.face = TOP then
							contour := terminal.pad_shape_tht.top;
						else
							contour := terminal.pad_shape_tht.bottom;
						end if;
						mirror_and_rotate;
						finalize;

					when OUTER_BOTTOM =>
						if terminal_position.face = BOTTOM then
							contour := terminal.pad_shape_tht.top;
						else
							contour := terminal.pad_shape_tht.bottom;
						end if;
						mirror_and_rotate;
						finalize;
				end case;
				

			when SMT =>
				if layer_category = OUTER_TOP and terminal_position.face = TOP then
					contour := terminal.pad_shape_smt;
					rotate_by (contour, to_rotation (terminal_position.rotation));
					finalize;						
					
				elsif layer_category = OUTER_BOTTOM and terminal_position.face = BOTTOM then
					contour := terminal.pad_shape_smt;
					mirror (contour, MIRROR_ALONG_Y_AXIS);
					rotate_by (contour, - to_rotation (terminal_position.rotation));
					finalize;
				end if;
		end case;


		if exists then
			return (
				exists		=> TRUE, 
				polygon		=> result, 
				position	=> terminal_position);
		else
			return (exists => FALSE);
		end if;

	end to_polygon;

	
	
end et_device_query_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
