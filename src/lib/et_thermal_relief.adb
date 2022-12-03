------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           THERMAL RELIEF                                 --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
--   to do:


with ada.text_io;				use ada.text_io;


package body et_thermal_relief is
	
	function to_string (connection : in type_pad_connection) return string is begin
		return to_lower (type_pad_connection'image (connection));
	end;

	function to_pad_connection (connection : in string) return type_pad_connection is begin
		return type_pad_connection'value (connection);
	end;

	
	function to_string (technology : in type_pad_technology) return string is begin
		return to_lower (type_pad_technology'image (technology));
	end;

	function to_pad_technology (technology : in string) return type_pad_technology is begin
		return type_pad_technology'value (technology);
	end to_pad_technology;
	

														 

	function make_relief (
		zone				: in type_zone'class;
		relief_properties	: in type_relief_properties;					 
		terminal_cursor		: in pac_terminals_with_relief.cursor;
		zone_clearance		: in type_track_clearance;
		zone_linewidth		: in type_track_width;
		debug				: in boolean := false)
		return type_relief
	is
		use pac_terminals;
		use pac_terminals_with_relief;
		terminal : type_terminal_with_relief renames element (terminal_cursor);

		zone_clearance_float : constant type_float_internal_positive := 
			type_float_internal_positive (zone_clearance);

		zone_linewidth_half_float : constant type_float_internal_positive :=
			0.5 * type_float_internal_positive (zone_linewidth);
		
		center : type_vector renames terminal.position.place;
		outline : type_polygon renames terminal.outline;
		
		
		angle : type_angle := terminal.position.rotation;
		relief : type_relief;
			
	begin
		relief.width := zone_linewidth;

		if debug then
			new_line;
			put_line ("terminal " & to_string (key (terminal.terminal))
				& " pos. " & to_string (terminal.position.place)
				& " angle " & to_string (terminal.position.rotation));
		end if;

		
		for i in 1 .. 4 loop
			if debug then
				put_line ("direction " & to_string (angle));
			end if;
				
			declare
				-- Get the distance from center of terminal to the 
				-- conducting area in the current direction:
				D2CA : constant type_distance_to_conducting_area := 
					--get_distance_to_conducting_area (zone, center, angle, debug);
					get_distance_to_conducting_area (zone, center, angle);

				-- The distance from edge to centerline:
				border_to_centerline : type_float_internal_positive;

				-- The actual gap between terminal and conducting area:
				gap : type_float_internal_positive;
				
			begin
				-- NOTE: There is no need to test whether the center of the terminal
				-- is in the non-conducting area of the zone.

				-- If no centerline exists in the current direction,
				-- then no spoke will be computed:
				if D2CA.centerline_exists then

					if debug then
						put_line ("distance to conducting area:");
						
						put_line ("to edge " 
							& to_string (D2CA.distance_to_edge));

						put_line ("to centerline of border " 
							& to_string (D2CA.distance_to_centerline));

						put_line ("gap max " & to_string (relief_properties.gap_max));
					end if;

					-- The spoke length is the distance from center of terminal
					-- to centerline of border.
					-- If the resulting gap between terminal and conducting area
					-- is less than the gap_max of the relief_properties, then
					-- append the spoke to the relief:
					border_to_centerline := D2CA.distance_to_centerline - D2CA.distance_to_edge;
					
					gap := D2CA.distance_to_centerline - border_to_centerline
						   - get_distance_to_border (outline, center, angle);
					
					if gap <= type_float_internal_positive (relief_properties.gap_max) then
						
						relief.spokes.append ((
							start_point	=> center,
							end_point	=> move_by (center, angle, D2CA.distance_to_centerline)));
					end if;
				end if;

				angle := angle + 90.0;
			end;
		end loop;

		return relief;
	end make_relief;

	
	function make_reliefes (
		zone				: in type_zone'class;
		relief_properties	: in type_relief_properties;
		terminals			: in pac_terminals_with_relief.list;
		zone_clearance		: in type_track_clearance;
		zone_linewidth		: in type_track_width)
		return pac_reliefes.list
	is
		result : pac_reliefes.list;

		debug : boolean := false;
		
		use pac_terminals_with_relief;

		procedure query_terminal (c : in pac_terminals_with_relief.cursor) is
		begin
			result.append (make_relief (
				zone, relief_properties, c, zone_clearance, zone_linewidth, debug));
		end query_terminal;
		
	begin
		terminals.iterate (query_terminal'access);
		return result;
	end make_reliefes;

	
	
end et_thermal_relief;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
