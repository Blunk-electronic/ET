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
		zone			: in type_zone'class;
		terminal_cursor	: in pac_terminals_with_relief.cursor;
		zone_clearance	: in type_track_clearance;
		zone_linewidth	: in type_track_width;
		debug			: in boolean := false)
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
		
		spoke_length_min : type_float_internal_positive;
		angle : type_angle := terminal.position.rotation;
		relief : type_relief;
			
	begin
		relief.width := zone_linewidth;

		if debug then
			put_line ("terminal " & to_string (key (terminal.terminal))
				& " pos. " & to_string (terminal.position.place)
				& " angle " & to_string (terminal.position.rotation));
		end if;

		
		for i in 1 .. 4 loop
			if debug then
				put_line ("direction " & to_string (angle));
			end if;
				
			declare
				distance_to_conducting_area : constant type_distance_to_conducting_area := 
					--get_distance_to_conducting_area (zone, center, angle, debug);
					get_distance_to_conducting_area (zone, center, angle);

				base_distance : type_float_internal_positive;
			begin
				if distance_to_conducting_area.exists then

					if debug then
						put_line ("distance to conducting area " 
							& to_string (distance_to_conducting_area.distance));
					end if;

					base_distance := get_distance_to_border (outline, center, angle)
						+ zone_linewidth_half_float;
					
					-- distance_to_conducting_area.distance
					-- zone.relief_properties.gap
					
					spoke_length_min := base_distance + zone_clearance_float;

					relief.spokes.append ((
						start_point	=> center,
						end_point	=> move_by (center, angle, spoke_length_min)));

				end if;
				angle := angle + 90.0;
			end;
		end loop;

		return relief;
	end make_relief;

	
	function make_reliefes (
		zone			: in type_zone'class;
		terminals		: in pac_terminals_with_relief.list;
		zone_clearance	: in type_track_clearance;
		zone_linewidth	: in type_track_width)
		return pac_reliefes.list
	is
		result : pac_reliefes.list;

		debug : boolean := true;
		
		use pac_terminals_with_relief;

		procedure query_terminal (c : in pac_terminals_with_relief.cursor) is
		begin
			result.append (make_relief (zone, c, zone_clearance, zone_linewidth, debug));
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
