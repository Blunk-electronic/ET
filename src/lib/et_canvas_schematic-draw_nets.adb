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

with et_text;
with et_schematic;				use et_schematic;

use et_schematic.type_nets;
use et_schematic.type_strands;
use et_schematic.type_net_segments;

separate (et_canvas_schematic)

procedure draw_nets (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	procedure draw_tag_label (
		net		: in et_general.type_net_name.bounded_string;
		label	: in type_net_label) is
		use et_text;
	begin
		
		pac_draw_misc.draw_text (
			area		=> in_area,
			context		=> context,
			content		=> to_content (to_string (net)),
			-- CS append to content the position of the net on the next sheet
			
			size		=> label.size,
			position	=> label.position,
			origin		=> true, -- CS must be false on export to image
			
			-- Text rotation around its anchor point.
			-- This is documentational text.
			-- It is readable from the front or the right.
			rotation	=> label.rotation_tag,
			alignment	=> (others => <>), -- CS
			height		=> self.drawing.frame_bounding_box.height
			);

	-- CS paint a box around the text depending on element (c).direction
	end draw_tag_label;
	
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
					use et_text;
					use pac_text;
				begin
					--put_line ("label at" & to_string (element (c).position));

					case element (c).appearance is
						when SIMPLE =>
							pac_draw_misc.draw_text (
								area		=> in_area,
								context		=> context,
								content		=> to_content (to_string (key (net_cursor))),
								size		=> element (c).size,
								position	=> element (c).position,
								origin		=> true, -- CS must be false on export to image
								
								-- Text rotation around its anchor point.
								-- This is documentational text.
								-- It is readable from the front or the right.
								rotation	=> to_rotation (element (c).rotation_simple),
								alignment	=> (others => <>), -- CS
								height		=> self.drawing.frame_bounding_box.height
								);

						when TAG =>
							draw_tag_label (net_name, element (c));

					end case;
				end query_label; 
					
			begin -- query_segments
				-- draw nets of the active sheet only:
				if strand.position.sheet = self.drawing.sheet then
					
					while segment_cursor /= type_net_segments.no_element loop

						pac_draw_misc.draw_line (
							area		=> in_area,
							context		=> context,
							line		=> element (segment_cursor),
							height		=> self.drawing.frame_bounding_box.height
							);

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
