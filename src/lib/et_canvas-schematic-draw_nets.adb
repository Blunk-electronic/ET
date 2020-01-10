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
				function make_bounding_box (segment : in type_net_segment) return type_model_rectangle is
					smallest_y, smallest_x, greatest_y, greatest_x : type_model_coordinate;

					-- The bounding box exists in the model. Therefore the drawing x and y values
					-- must be converted to model coordinates:
					start_point_x	: type_model_coordinate := convert_x (segment.start_point.x);
					start_point_y	: type_model_coordinate := convert_y (segment.start_point.y);
					end_point_x		: type_model_coordinate := convert_x (segment.end_point.x);
					end_point_y		: type_model_coordinate := convert_y (segment.end_point.y);
				begin
					-- NOTE: Model coordinates have the y-axis increasing downwards !

					if start_point_y < end_point_y then
						smallest_y := start_point_y;
						greatest_y := end_point_y;
					else
						smallest_y := end_point_y;
						greatest_y := start_point_y;
					end if;

					if start_point_x < end_point_x then
						smallest_x := start_point_x;
						greatest_x := end_point_x;
					else
						smallest_x := end_point_x;
						greatest_x := start_point_x;
					end if;

					-- CS include net labels in the bounding box
					
					return (
							x => smallest_x, y => smallest_y, -- position
							width => greatest_x - smallest_x, -- width
							height => greatest_y - smallest_y -- height
						   );
				end make_bounding_box;
				
			begin -- query_segments
				
				-- draw nets of the active sheet only:
				if strand.position.sheet = model.sheet then
					
					while segment_cursor /= type_net_segments.no_element loop

						-- We draw the segment if:
						--  - no area given or
						--  - if the bounding box of the segment intersects the given area
						if (in_area = no_rectangle
							or else intersects (in_area, make_bounding_box (element (segment_cursor)))) 
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
								type_view_coordinate (model.frame_bounding_box.x),
								type_view_coordinate (model.frame_bounding_box.y));

							cairo.set_line_width (context.cr, 1.0);

							cairo.set_source_rgb (context.cr, gdouble (0), gdouble (1), gdouble (0)); -- green

							
							-- start point
							cairo.move_to (
								context.cr,
								convert_x (element (segment_cursor).start_point.x),
								convert_y (element (segment_cursor).start_point.y)
								);

							-- end point
							cairo.line_to (
								context.cr,
								convert_x (element (segment_cursor).end_point.x),
								convert_y (element (segment_cursor).end_point.y)
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
