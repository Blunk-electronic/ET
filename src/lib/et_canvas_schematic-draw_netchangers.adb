------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       SCHEMATIC / DRAW NETCHANGERS                       --
--                                                                          --
--                               B o d y                                    --
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
-- To Do:
--
--


with ada.text_io;						use ada.text_io;

with et_colors;							use et_colors;

with et_net_linewidth;					use et_net_linewidth;
with et_netchanger_symbol_schematic;	use et_netchanger_symbol_schematic;
with et_netchangers;					use et_netchangers;
with et_netchangers.schematic;			use et_netchangers.schematic;
with et_primitive_objects;				use et_primitive_objects;
with et_alignment;						use et_alignment;
with et_display.schematic;				use et_display.schematic;
with et_symbol_port_measures;			use et_symbol_port_measures;
with et_symbol_ports;					use et_symbol_ports;
with et_symbol_text;					use et_symbol_text;
with et_text_content;					use et_text_content;


separate (et_canvas_schematic)


procedure draw_netchangers is

	use et_colors.schematic;
	
	brightness : type_brightness := NORMAL;


	-- This procedure draws the line of a port candidate.
	-- The start of the line is where a net segment is attached to.
	-- The end of the line points toward the body of the netchanger.
	-- Depending on the rotation of the port (as defined in the
	-- netchanger symbol) the start (A) and end (B) of the line is computed here:
	procedure draw_port (
		name 				: in type_netchanger_port_name;
		port				: in type_netchanger_port;
		netchanger_position	: in type_position)
	is
	
		-- For the rotation of port and port name, the
		-- total rotation is required:
		rotation_total : constant type_rotation := 
			add (port.rotation, get_rotation (netchanger_position));
		
		-- This is the start point of the line:
		A : type_vector_model := port.position;

		-- This is the end point of the line.
		-- It will be computed according to the rotation of
		-- the port and the length of the line (port.length):
		B : type_vector_model := port.position;

		-- The position of the origin of the port name:
		pos_port_name : type_vector_model;

	
	
		-- Compute the following positions according to rotation and 
		-- length of the port:
		-- - start and end point of the line (The end points towards 
		--   the netchanger body.)
		-- - From start and end of the line (A/B) the preliminary 
		--   position of the port name is computed.
		--
		-- NOTE: Regarding the position of the port name:
		-- For the moment, the computations below leave the rotation of the 
		-- netchanger in the schematic outside. For the moment we assume 
		-- that the netchanger is not rotated in the schematic. We look 
		-- at the default rotation of the ports as they are defined in 
		-- the netchanger symbol.
		-- The final coordinates of the port name will be computed later.
		procedure compute_positions is begin
			if port.rotation = 0.0 then -- end point points to the left
				-- Compute the end point. It is left of the start point:
				set (axis => AXIS_X, value => get_x (A) - port.length, point => B);

				-- Compute the position of the port name. 
				-- It is some distance left of the start point:
				pos_port_name := A;
				set (axis => AXIS_X, 
					value => get_x (A) - port_name_spacing_start, point => pos_port_name);
				
			elsif port.rotation = 180.0 then  -- end point points to the left
				-- Compute the end point. It is right of the start point:
				set (axis => AXIS_X, value => get_x (A) + port.length, point => B);

				-- Compute the position of the port name. 
				-- It is some distance right of the start point:
				pos_port_name := A;
				set (axis => AXIS_X, 
					value => get_x (A) + port_name_spacing_start, point => pos_port_name);
				
			else
				raise constraint_error; -- CS do something helpful. should never happen
			end if;	
		end compute_positions;
		
		
		
		
		-- Draws the line and the circle of the port:
		procedure draw_line_and_circle is 
			-- The line that represents the port:
			line : type_line;

			-- The circle at the start of the line where net
			-- segments are attached to:
			circle : type_circle;
		begin
			-- Set start and and points of the line:
			set_A (line, A);
			set_B (line, B);

			-- Draw the line:
			set_color_symbols (brightness);
			
			draw_line (line, netchanger_position, net_linewidth,
				stroke	=> DO_STROKE);

			-- Draw the circle around the start point
			-- of the line if the port-layer is enabled:
			if ports_enabled then
				-- put_line ("draw port");
				
				-- The start point of the port must have a small green circle around it.
				-- set color and line width
				set_color_ports (brightness);

				-- Set center and radius of the circle:
				set_center (circle, get_A (line));
				set_radius (circle, port_circle_radius);

				-- Draw the circle. It is not filled:
				draw_circle (
					circle	=> circle, 
					pos		=> netchanger_position, 
					filled	=> NO,
					width	=> port_circle_linewidth, 
					stroke	=> DO_STROKE);
				
			end if;
		end draw_line_and_circle;

		
		
	
		-- This procedure draws the port name at its final
		-- position taking into account the rotation of 
		-- the netchanger in the schematic:
		procedure draw_port_name is
			use pac_text;

			-- The vertical alignment is untouched and is always BOTTOM.
			-- The horizontal alignment depends on the total rotation
			-- which is a sum of port rotation and netchanger rotation.
			use et_alignment;
			alignment : type_text_alignment := (
				horizontal => ALIGN_CENTER, vertical => ALIGN_BOTTOM);

			use pac_draw_text;
		begin
			-- Rotate the position of the port name by the 
			-- rotation of the netchanger:
			rotate_by (pos_port_name, get_rotation (netchanger_position));

			-- Move the name by the netchanger position:
			move_by (pos_port_name, get_place (netchanger_position));

			
			-- Now some fine adjustment is required to place the port
			-- name some distance away from the line of the port.
			-- Compute the position of the origin of the port name regarding 
			-- its distance from the line of the port:
			if rotation_total = 0.0 or rotation_total = 360.0 or rotation_total = -360.0 then
				-- The line is horizontal. So the port name must be 
				-- moved up above the line by some distance:
				move (pos_port_name, DIR_UP, terminal_name_spacing_line);
				alignment.horizontal := ALIGN_RIGHT;

			elsif rotation_total = 90.0 or rotation_total = -270.0 then
				-- The line is vertical. So the port name must be 
				-- moved left of the line by some distance:
				move (pos_port_name, DIR_LEFT, terminal_name_spacing_line);
				alignment.horizontal := ALIGN_RIGHT;
				
			elsif rotation_total = 180.0 or rotation_total = -180.0 then
				-- The line is horizontal. So the port name must be 
				-- moved up above the line by some distance:
				move (pos_port_name, DIR_UP, terminal_name_spacing_line);
				alignment.horizontal := ALIGN_LEFT;
				
			elsif rotation_total = -90.0 or rotation_total = 270.0 then
				-- The line is vertical. So the port name must be 
				-- moved left of the line by some distance:
				move (pos_port_name, DIR_LEFT, terminal_name_spacing_line);
				alignment.horizontal := ALIGN_LEFT;
				
			else
				raise constraint_error; -- CS should never happen
			end if;
			-- CS: move to a procedure
			
			
			set_color_symbols (brightness);

			-- CS move to a procedure
			draw_text (
				content		=> to_content (to_string (name)),
				size		=> port_size,
				font		=> et_symbol_text.text_font,
				anchor		=> pos_port_name,
				origin		=> false,  -- no origin required

				-- Text rotation about its anchor point.
				-- This is documentational text. Its rotation must
				-- be snapped to either HORIZONAL or VERTICAL so that
				-- it is readable from the front or the right.
				rotation	=> to_rotation (to_rotation_doc (rotation_total)),
				alignment	=> alignment);

		end draw_port_name;

	
	begin
		compute_positions;
		draw_line_and_circle;	
		draw_port_name;
	end draw_port;

	
	
	
	
	-- This procedure draws the name of the netchanger (like N31).
	-- It takes the index (like 31) and the position of the
	-- netchanger. 
	-- Depending on the rotation of the netchanger the name
	-- is offset slightly to a new position relative to the
	-- center of the netchanger.
	-- The name is fixed relative to the origin of the netchanger:
	procedure draw_name (
		index		: in type_netchanger_id; -- 31
		position	: in type_position) -- incl. x/x/rotatoiin
	is
		use pac_text;
		use pac_draw_text;
		use et_alignment;
		
		alignment : type_text_alignment := (
			horizontal => ALIGN_CENTER, vertical => ALIGN_CENTER);

		-- The rotation of the netchanger:	
		rotation : type_rotation := get_rotation (position);
		
		-- The final position of the name.
		-- Initially it is the same as the netchanger position
		-- (where its center/origin is):
		pos_final : type_vector_model := get_place (position);
		
		
		
		-- This procedure computes the final position of
		-- the name (like N31) depending on the rotation
		-- of the netchanger:		 
		procedure compute_position is begin
			if rotation = 0.0 or rotation = 360.0 or rotation = -360.0 then
				-- The netchanger is horizontal. 
				-- So the name must be moved down:
				move (pos_final, DIR_DOWN, name_to_origin_offset);

			elsif rotation = 90.0 or rotation = -270.0 then
				-- The netchanger is vertical. 
				-- So the name must be moved right:
				move (pos_final, DIR_RIGHT, name_to_origin_offset);
				
			elsif rotation = 180.0 or rotation = -180.0 then
				-- The netchanger is horizontal but upside-down. 
				-- So the name must be moved up:
				move (pos_final, DIR_UP, name_to_origin_offset);
				
			elsif rotation = -90.0 or rotation = 270.0 then
				-- The netchanger is vertical. 
				-- So the name must be moved left:
				move (pos_final, DIR_LEFT, name_to_origin_offset);
				
			else
				raise constraint_error; -- CS should never happen
			end if;
		end compute_position;

		

		-- This procedure does the final drawing of the name:
		procedure do_draw is begin	
			set_color_placeholders (brightness);
			
			draw_text (
				content		=> to_content (get_netchanger_name (index)),
				size		=> name_size,
				font		=> et_symbol_text.text_font,
				anchor		=> pos_final,
				origin		=> false,  -- no origin required

				-- Text rotation about its anchor point.
				-- This is documentational text. Its rotation must
				-- be snapped to either HORIZONAL or VERTICAL so that
				-- it is readable from the front or the right.
				rotation	=> to_rotation (to_rotation_doc (get_rotation (position))),
				alignment	=> alignment);
		end do_draw;

		
	begin		
		compute_position;		
		do_draw;		
	end draw_name;
	
	 
	
	

	-- This procedure draws the body of the netchanger
	-- at the given position. Currently the body consists
	-- of just a single arc:
	procedure draw_body (
		position : in type_position)
	is begin
		set_color_symbols (brightness);
		
		draw_arc (
			arc		=> netchanger_symbol.arc,
			pos		=> position,
			width	=> net_linewidth,
			stroke	=> DO_STROKE);
	
	end draw_body;

	
	
	

	-- This procedure iterates through all netchangers
	-- of the active module and draws them:	 
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is 
		use pac_netchangers;
		netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;


		procedure query_netchanger (
			index		: in type_netchanger_id;
			netchanger	: in type_netchanger)
		is		
			-- Get the position of the given netchanger.
			-- NOTE: A netchanger can only assume a rotation of 0 or 90 degree
			-- by its specification. This is a conversion from netchanger position
			-- to a regular position (x/y/rotation):
			position : type_position := to_position (netchanger.position_sch);
		begin
			-- CS being moved
			-- CS selected

			-- Draw the body of the netchanger:
			draw_body (position);
						
			-- Draw the ports of the netchanger:
			draw_port (name => MASTER, port => netchanger_symbol.master_port, 
					netchanger_position => position);

			draw_port (name => SLAVE, port => netchanger_symbol.slave_port, 
					netchanger_position => position);

			-- Draw the name of the netchanger (like N33):
			draw_name (index => index, position => position);
					
			-- Draw the origin of the netchanger:
			set_color_origin (brightness);
			draw_origin ((get_place (position), 0.0));
			-- NOTE: The origin is never rotated.
			
		end query_netchanger;
		
									   
	begin		
		-- Iterate through the netchangers of the module:
		while has_element (netchanger_cursor) loop
			query_element (netchanger_cursor, query_netchanger'access);
			next (netchanger_cursor);
		end loop;
	end query_module;

	
	
begin
-- 	put_line ("draw netchangers (schematic)");


	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_module'access);

	-- CS: draw_netchanger_being_added
	
end draw_netchangers;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
