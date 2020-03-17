------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC DRAW NETS                              --
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

separate (et_canvas_schematic)

procedure draw_nets (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
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
				junction : type_junction_symbol := junction_symbol;

				procedure draw_junction is begin
					pac_draw_misc.draw_circle (
						area		=> in_area,
						context		=> context,
						circle		=> junction,
						filled		=> YES,
						height		=> self.drawing.frame_bounding_box.height);
				end draw_junction;

				procedure query_label (c : in type_net_labels.cursor) is 
					use type_net_labels;
				begin
					put_line ("label at" & to_string (element (c).position));

-- 					pac_draw_misc.draw_text 
-- 						(
-- 						context		=> context,
-- 						text		=> pac_text.type_text (element (c)),
-- 						content		=> element (c).content,
-- 						size		=> element (c).size,
-- 
-- 						-- text position x/y relative to symbol origin:
-- 						x			=> transpose_x (x (position)),
-- 						y			=> transpose_y (y (position)),
-- 
-- 						-- Text rotation around its anchor point.
-- 						-- This is documetational text. Its rotation must
-- 						-- be snapped to either HORIZONAL or VERTICAL so that
-- 						-- it is readable from the front or the right.
-- 						rotation	=> to_rotation (snap (element (c).rotation + unit_rotation)),
-- 
-- 						alignment	=> element (c).alignment
-- 						);


					
				end query_label; 
					
			begin -- query_segments
				-- draw nets of the active sheet only:
				if strand.position.sheet = self.drawing.sheet then
					
					while segment_cursor /= type_net_segments.no_element loop

						pac_draw_misc.draw_line (
							area		=> in_area,
							context		=> context,
							line		=> element (segment_cursor),
							height		=> self.drawing.frame_bounding_box.height,
							extend_boundaries	=> false,
							boundaries_to_add	=> boundaries_default -- CS
							);

						-- CS include net labels (if any) in the boundaries

						-- draw junctions:
						
						-- at start point of segment:
						if element (segment_cursor).junctions.start_point then
							junction.center := element (segment_cursor).start_point;
							draw_junction;
						end if;

						-- at end point of segment:
						if element (segment_cursor).junctions.end_point then
							junction.center := element (segment_cursor).end_point;
							draw_junction;
						end if;

						-- draw labels
						type_net_labels.iterate (element (segment_cursor).labels, query_label'access);
						
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
-- 		save (context.cr);
-- 		
-- 		-- Prepare the current transformation matrix (CTM) so that
-- 		-- all following drawing is relative to the upper left frame corner.
-- 		translate (
-- 			context.cr,
-- 			convert_x (self.drawing.frame_bounding_box.x),
-- 			convert_y (self.drawing.frame_bounding_box.y));

		cairo.set_line_width (context.cr, type_view_coordinate (et_schematic.net_line_width));
		cairo.set_source_rgb (context.cr, gdouble (0), gdouble (1), gdouble (0)); -- green

		-- iterate nets
		while net_cursor /= type_nets.no_element loop

			type_nets.query_element (
				position	=> net_cursor,
				process		=> query_strands'access);

			next (net_cursor);
		end loop;

		cairo.stroke (context.cr);

-- 		restore (context.cr);
	end query_nets;
	
begin
-- 	put_line ("draw nets ...");
-- 	put_line (to_string (in_area));
	
	-- draw the nets
	type_modules.query_element (
		position	=> current_active_module,
		process		=> query_nets'access);
	
end draw_nets;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
