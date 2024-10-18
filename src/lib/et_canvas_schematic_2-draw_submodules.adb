------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           DRAW SUBMODULES                                --
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

with ada.text_io;				use ada.text_io;

with et_geometry;				use et_geometry;
with et_text;					use et_text;
with et_nets;
with et_net_names;				use et_net_names;
with et_submodules;				use et_submodules;
with et_display.schematic;

with et_pcb_coordinates_2;


separate (et_canvas_schematic_2)


procedure draw_submodules is
	
	use pac_net_name;
	use pac_submodules;

	use et_colors;
	use et_colors.schematic;
	
	
	procedure query_submods (cursor : in pac_submodules.cursor) is
		submod : type_submodule renames element (cursor);
		
		
		procedure draw_box is 
			box : type_area;
		begin
			set_color_submodules;

			-- The lower left corner of the submodule box in the schematic:
			box.position := submod.position.place;
			box.width  := submod.size.x;
			box.height := submod.size.y;
			
			draw_rectangle (
				rectangle	=> box,
				width		=> submod_box_line_width);
								
		end draw_box;

		
		-- The position of the instance name is below the lower left corner of the box.
		-- Position and size are fixed and can not be changed by the operator:
		procedure draw_instance_name is
			position : type_vector_model := submod.position.place;
			
			offset : constant type_distance_relative := to_distance_relative (set (
					x => zero,
					y => - text_spacing));

			use pac_draw_text;
		begin
			move_by (position, offset);
			
			draw_text (
				content		=> to_content ("instance: " & to_string (key (cursor))),
				size		=> instance_font_size,
				font		=> instance_font,
				anchor		=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (LEFT, TOP));

		end draw_instance_name;

		
		-- The position of the file name is below the instance name.
		-- Position and size are fixed and can not be changed by the operator:
		procedure draw_file_name is
			position : type_vector_model := submod.position.place;
			
			offset : constant type_distance_relative := to_distance_relative (set (
					x => zero,
					y => - (2.0 * text_spacing + instance_font_size)));

			use pac_draw_text;
		begin
			move_by (position, offset);
			
			draw_text (
				content		=> to_content ("file: " & to_string (element (cursor).file)),
				size		=> file_font_size,
				font		=> file_font,
				anchor		=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (LEFT, TOP));

		end draw_file_name;

		
		-- The position of module in the board is below the file name.
		-- Position and size are fixed and can not be changed by the operator:
		procedure draw_position_in_board is
			position : type_vector_model := submod.position.place;
			
			offset : constant type_distance_relative := to_distance_relative (set (
					x => zero,
					y => - (3.0 * text_spacing + instance_font_size + file_font_size)));

			use et_pcb_coordinates_2.pac_geometry_2;
			
			pos_x : constant string := to_string (get_x (element (cursor).position_in_board));
			pos_y : constant string := to_string (get_y (element (cursor).position_in_board));
			rotation : constant string := to_string (get_rotation (element (cursor).position_in_board));
			
			text : constant string := "board (x/y/rot.):" &
					pos_x & axis_separator &
					pos_y & axis_separator &
					rotation;

			use pac_draw_text;
		begin
			move_by (position, offset);
			
			draw_text (
				content		=> et_text.to_content (text),
				size		=> position_board_font_size,
				font		=> position_board_font,
				anchor		=> position,
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (LEFT, TOP));

		end draw_position_in_board;

		
		
		procedure draw_ports is 
			use pac_submodule_ports;

			
			procedure draw_port (pc : in pac_submodule_ports.cursor) is 
				port : type_submodule_port renames element (pc);
				
				submod_position : constant type_vector_model := submod.position.place;

				-- The final position of the port:
				pos : type_vector_model := submod_position;

				
				procedure draw_horizontal is 
					box : type_area;
				begin
					box.position := pos;
					box.width  := port_symbol_width;
					box.height := port_symbol_height;
					
					-- Draw the port horizontal:
					draw_rectangle (rectangle => box, width => port_symbol_line_width);
				end draw_horizontal;

				
				procedure draw_vertical is 
					box : type_area;
				begin
					box.position := pos;
					box.width  := port_symbol_height;
					box.height := port_symbol_width;

					-- Draw the port vertical:					
					draw_rectangle (rectangle => box, width => port_symbol_line_width);
				end draw_vertical;
				

				use pac_draw_text;
				
			begin -- draw_port
				-- Detect the edge where the port sits at. Depending on the edge
				-- the port must be drawn 0, 90, 180 or 270 degree.

				-- Move pos by the position of the port. 
				-- The port position is relative to the module (box) position:
				move_by (pos, to_distance_relative (port.position));

				-- According to the edge where the port sits, pos will now be fine
				-- adjusted, because the port is a rectangle which position is at 
				-- its lower left corner.
				
				-- Does the port sit on the LEFT edge of the box ?
				if get_x (port.position) + get_x (submod_position) = get_x (submod_position) then 

					-- Draw the port direction (the letter M or S) inside the port rectangle:
					draw_text (
						content		=> to_content (to_direction_abbrevation (port.direction)),
						size		=> port_direction_font_size,
						font		=> port_direction_font,
						anchor		=> move (pos, 0.0, port_symbol_width / 2.0),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (CENTER, CENTER));
					
					-- Draw the port name. The text is placed on the RIGHT of the port rectangle:
					draw_text (
						content		=> to_content (to_string (key (pc))),
						size		=> port_name_font_size,
						font		=> port_name_font,
						anchor		=> move (pos, 0.0, port_symbol_width + port_name_spacing),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (LEFT, CENTER));
					
					-- Move pos down so that the port sits excatly at
					-- the point where a net will be connected:
					move_by (pos, to_distance_relative (set (x => zero, y => - port_symbol_height / 2.0)));

					draw_horizontal;

					
				-- Does the port sit on the RIGHT edge of the box ?
				elsif get_x (port.position) + get_x (submod_position) = get_x (submod_position) + element (cursor).size.x then 

					-- Draw the port direction (the letter M or S) inside the port rectangle:
					draw_text (
						content		=> to_content (to_direction_abbrevation (port.direction)),
						size		=> port_direction_font_size,
						font		=> port_direction_font,
						anchor		=> move (pos, 180.0, port_symbol_width / 2.0),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (CENTER, CENTER));
					
					-- Draw the port name. The text is placed on the LEFT of the port rectangle:
					draw_text (
						content		=> to_content (to_string (key (pc))),
						size		=> port_name_font_size,
						font		=> port_name_font,
						anchor		=> move (pos, 180.0, port_symbol_width + port_name_spacing),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (RIGHT, CENTER));
					
					-- Move pos down and left so that the port sits excatly at
					-- the point where a net will be connected:
					move_by (pos, to_distance_relative (set (x => - port_symbol_width, y => - port_symbol_height / 2.0)));

					draw_horizontal;

					
				-- Does the port sit on the LOWER edge of the box ?
				elsif get_y (port.position) + get_y (submod_position) = get_y (submod_position) then

					-- Draw the port direction (the letter M or S) inside the port rectangle:
					draw_text (
						content		=> to_content (to_direction_abbrevation (port.direction)),
						size		=> port_direction_font_size,
						font		=> port_direction_font,
						anchor		=> move (pos, 90.0, port_symbol_width / 2.0),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (CENTER, CENTER));
					
					-- Draw the port name. The text is placed ABOVE the port rectangle:
					draw_text (
						content		=> to_content (to_string (key (pc))),
						size		=> port_name_font_size,
						font		=> port_name_font,
						anchor		=> move (pos, 90.0, port_symbol_width + port_name_spacing),
						origin		=> false, -- no origin required
						rotation	=> 90.0,
						alignment	=> (LEFT, CENTER));
					
					-- Move pos left so that the port sits excatly at
					-- the point where a net will be connected:
					move_by (pos, to_distance_relative (set (x => - port_symbol_height / 2.0, y => zero)));

					draw_vertical;

					
				-- Does the port sit on the UPPER edge of the box ?
				elsif get_y (port.position) + get_y (submod_position) = get_y (submod_position) + element (cursor).size.y then 

					-- Draw the port direction (the letter M or S) inside the port rectangle:
					draw_text (
						content		=> to_content (to_direction_abbrevation (port.direction)),
						size		=> port_direction_font_size,
						font		=> port_direction_font,
						anchor		=> move (pos, 270.0, port_symbol_width / 2.0),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (CENTER, CENTER));
					
					-- Draw the port name. The text is placed BELOW the port rectangle:
					draw_text (
						content		=> to_content (to_string (key (pc))),
						size		=> port_name_font_size,
						font		=> port_name_font,
						anchor		=> move (pos, 270.0, port_symbol_width + port_name_spacing),
						origin		=> false, -- no origin required
						rotation	=> 90.0,
						alignment	=> (RIGHT, CENTER));
					
					-- Move pos up and left so that the port sits excatly at
					-- the point where a net will be connected:
					move_by (pos, to_distance_relative (set (x => - port_symbol_height / 2.0, y => - port_symbol_width)));

					draw_vertical;
					
				else
					-- port does not sit on any edge
					raise constraint_error; -- CS should never happen
				end if;

			end draw_port;

			
		begin -- draw_ports
			iterate (element (cursor).ports, draw_port'access);
		end draw_ports;

		
		use et_display.schematic;

		
	begin -- query_submods
		-- We want to draw only those submodules which are on the active sheet:
		if get_sheet (element (cursor).position) = active_sheet then

			draw_box;
			draw_ports;

			-- Draw file and instance name and position in board if
			-- layer device_names is enabled:
			if device_names_enabled then

				set_color_placeholders;
				
				draw_file_name;
				draw_instance_name;
				draw_position_in_board;
			end if;

			-- CS view mode ?

		end if;
	end query_submods;

	
begin
-- 	put_line ("draw submodules ...");

	iterate (element (active_module).submods, query_submods'access);
	
end draw_submodules;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
