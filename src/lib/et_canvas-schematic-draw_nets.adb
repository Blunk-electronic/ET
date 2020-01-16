------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW NETS                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with cairo;						use cairo;
with pango.layout;				use pango.layout;

with et_general;				use et_general;
with et_project;				use et_project;
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

with et_schematic;				use et_schematic;
use et_schematic.type_nets;
use et_schematic.type_strands;
use et_schematic.type_net_segments;

separate (et_canvas.schematic)

procedure draw_nets (
	model	: not null access type_model;
	in_area	: in type_model_rectangle := no_rectangle;
	context : in type_draw_context) is

	procedure query_nets (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is

		net_cursor : type_nets.cursor := module.nets.first;

		procedure query_strands (
			net_name	: in et_general.type_net_name.bounded_string;
			net			: in type_net) is
			strand_cursor : type_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				segment_cursor : type_net_segments.cursor := strand.segments.first;

				-- The bounding box that surrounds the segment must be calculated.
				bounding_box : type_model_rectangle;

				procedure make_bounding_box (sc : in type_net_segments.cursor) is
					use pac_shapes;					
					boundaries : type_boundaries := pac_shapes.boundaries (type_line (element (sc)));
				begin
					bounding_box := (
						-- The bounding box origin is the upper left corner.
						-- The box position in x is the smallest_x.
						-- The box position in y is the greatest_y (upwards).
						-- The box position in y is additonally converted to y axis going downwards.
						x		=> convert_x (boundaries.smallest_x),
						y		=> convert_and_shift_y (boundaries.smallest_y),

						-- The box width is the difference between greatest x and smallest x.
						-- The box height is the difference between greatest y and smallest y.
						width	=> convert_x (boundaries.greatest_x - boundaries.smallest_x),
						height	=> convert_y (boundaries.greatest_y - boundaries.smallest_y)
						);

					-- CS include net labels in the bounding box
				end make_bounding_box;
				
			begin -- query_segments
				
				-- draw nets of the active sheet only:
				if strand.position.sheet = model.sheet then
					
					while segment_cursor /= type_net_segments.no_element loop

						make_bounding_box (segment_cursor);
						
						-- We draw the segment if:
						--  - no area given or
						--  - if the bounding box of the segment intersects the given area
						if (in_area = no_rectangle
							or else intersects (in_area, bounding_box)) 
						then
							-- CS test size 
					-- 			if not size_above_threshold (self, context.view) then
					-- 				return;
					-- 			end if;

							save (context.cr);

							-- Prepare the current transformation matrix (CTM) so that
							-- all following drawing is relative to the upper left frame corner.
							translate (
								context.cr,
								convert_x (model.frame_bounding_box.x),
								convert_y (model.frame_bounding_box.y));

							cairo.set_line_width (context.cr, type_view_coordinate (et_schematic.net_line_width));

							cairo.set_source_rgb (context.cr, gdouble (0), gdouble (1), gdouble (0)); -- green

							
							-- start point
							cairo.move_to (
								context.cr,
								convert_x (element (segment_cursor).start_point.x),
								convert_and_shift_y (element (segment_cursor).start_point.y)
								);

							-- end point
							cairo.line_to (
								context.cr,
								convert_x (element (segment_cursor).end_point.x),
								convert_and_shift_y (element (segment_cursor).end_point.y)
								);

							-- CS draw junctions and labels

							cairo.stroke (context.cr);
							restore (context.cr);
							
						end if;
						
						next (segment_cursor);
					end loop;

				end if;
			end query_segments;
			
		begin -- query_strands
			while strand_cursor /= type_strands.no_element loop

				query_element (
					position	=> strand_cursor,
					process		=> query_segments'access);
				
				next (strand_cursor);
			end loop;
		end query_strands;
		
	begin -- query_nets
		while net_cursor /= type_nets.no_element loop

			type_nets.query_element (
				position	=> net_cursor,
				process		=> query_strands'access);

			next (net_cursor);
		end loop;
		
	end query_nets;
	
begin
-- 	put_line ("draw nets ...");
	
	-- draw the nets
	type_modules.query_element (
		position	=> model.module,
		process		=> query_nets'access);
	
end draw_nets;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
