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

with ada.exceptions;

separate (et_canvas_schematic)

procedure draw_nets (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	use et_schematic;
	use et_schematic.type_nets;
	use et_schematic.type_strands;
	use et_schematic.type_net_segments;

	use pac_draw_misc;

	function is_selected (
		s : in type_net_segments.cursor)
		return boolean is
		use pac_proposed_segments;
	begin
		if is_empty (proposed_segments) then
			return false;
		else
			if selected_segment /= pac_proposed_segments.no_element then
				if s = element (selected_segment).segment then
					return true;
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;

-- 		exception
-- 			when event: others =>
-- 				log_indentation_reset;
-- 				log (text => "ABC", console => true);
-- 				log (text => ada.exceptions.exception_information (event), console => true);
-- 				raise;
		
	end is_selected;
	
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
					draw_circle (
						area		=> in_area,
						context		=> context,
						circle		=> junction,
						filled		=> YES,
						height		=> self.frame_height);
				end draw_junction;

				procedure query_label (c : in type_net_labels.cursor) is 
					use type_net_labels;
					use et_text;
					use pac_text;
				begin
					--put_line ("label at" & to_string (element (c).position));

					case element (c).appearance is
						when SIMPLE =>
							draw_text (
								area		=> in_area,
								context		=> context,
								content		=> to_content (to_string (key (net_cursor))),
								size		=> element (c).size,
								font		=> net_label_font,
								position	=> element (c).position,
								origin		=> true, -- CS must be false on export to image
								
								-- Text rotation around its anchor point.
								-- This is documentational text.
								-- It is readable from the front or the right.
								rotation	=> to_rotation (element (c).rotation_simple),
								alignment	=> (LEFT, BOTTOM),
								height		=> self.frame_height
								);

						when TAG =>
							draw_tag_label (self, in_area, context, net_name, element (c));

					end case;
				end query_label; 
					
			begin -- query_segments
				-- draw nets of the active sheet only:
				if strand.position.sheet = current_active_sheet then
					
					while segment_cursor /= type_net_segments.no_element loop

						-- set line width for net segments:
						set_line_width (context.cr, type_view_coordinate (et_schematic.net_line_width));

						if is_selected (segment_cursor) then
							set_color_nets (context.cr, BRIGHT);
						end if;
						
						-- draw the net segment:
						draw_line (
							area		=> in_area,
							context		=> context,
							line		=> element (segment_cursor),
							height		=> self.frame_height
							);

						-- draw labels
						type_net_labels.iterate (element (segment_cursor).labels, query_label'access);

						set_color_nets (context.cr, NORMAL);

						
						-- Draw junctions.
						-- There is no highlighting for junctions.
						
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
		set_color_nets (context.cr);

		-- iterate nets
		while net_cursor /= type_nets.no_element loop

			type_nets.query_element (
				position	=> net_cursor,
				process		=> query_strands'access);

			next (net_cursor);
		end loop;

	end query_nets;
	
begin
-- 	put_line ("draw nets ...");
-- 	put_line (to_string (in_area));
	
	-- draw the nets
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_nets'access);
	
end draw_nets;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
