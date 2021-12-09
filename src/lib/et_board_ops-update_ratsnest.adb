------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  BOARD OPERATIONS / UPDATE RATSNEST                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.strings;					use ada.strings;
with ada.exceptions;
--with ada.containers.multiway_trees;
with et_exceptions;					use et_exceptions;

--with et_routing;					use et_routing;

separate (et_board_ops)

procedure update_ratsnest (
	module_cursor	: in pac_generic_modules.cursor;
	lth				: in type_log_level)
is
	
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in out type_module) 
	is

		procedure query_net (net_cursor : in pac_nets.cursor) is

			use pac_points;
			points : pac_points.list;

			use pac_airwires;
			airwires_primary : pac_airwires.list;
			airwires_to_add : pac_airwires.list;
			airwires_to_delete : pac_airwires.list;


			-- The common start point of all primary airwires:
			root : type_point;
			
			procedure make_primary_airwire (p : in pac_points.cursor) is
			begin
				--if p /= points.first then
				if element (p) /= root then
					airwires_primary.append (type_line (make_line (root, element (p))));
				end if;
			end make_primary_airwire;


			function get_nearest_primary_airwire (a_in : in pac_airwires.cursor) 
				return pac_airwires.cursor
			is 
				smallest_distance : type_distance_positive := type_distance'last;
				
				nearest : pac_airwires.cursor := pac_airwires.no_element;

				procedure query_end_point (ca : in pac_airwires.cursor) is
					d_tmp : type_distance_positive;
				begin
					-- Query the end point of all primary airwires except the 
					-- given airwire indicted by cursor a_in:
					if a_in /= ca then

						-- Get the distance from end point of a_in to the end point of the
						-- candidate airwire indicated by cursor ca:
						d_tmp := get_absolute (get_distance (element (a_in).end_point, element (ca).end_point));
						--put_line (" end " & to_string (element (cp)) & " d " & to_string (d));

						-- The distance must be shorter than the longest of the two
						-- airwires:
						if d_tmp < get_greatest_length (element (a_in), element (ca)) then
							
							-- Update smallest_distance if current distance (d_tmp) is
							-- smaller than the old smallest_distance:
							if d_tmp < smallest_distance then
								smallest_distance := d_tmp;
								nearest := ca;
							end if;
							
						end if;
					end if;
				end query_end_point;


			begin
				airwires_primary.iterate (query_end_point'access);
				return nearest;
			end get_nearest_primary_airwire;
			

			
			procedure query_airwire_primary (a : in pac_airwires.cursor) is
				nearest : pac_airwires.cursor;
				aw_tmp : type_line;
			begin
				nearest := get_nearest_primary_airwire (a);

				if nearest /= pac_airwires.no_element then
					aw_tmp := (type_line (get_longest (element (a), element (nearest))));
					if not airwires_to_delete.contains (aw_tmp) then
						airwires_to_delete.append (aw_tmp);
					end if;

					aw_tmp := type_line (make_line (element (a).end_point, element (nearest).end_point));

					if not airwires_to_add.contains (aw_tmp) then
						airwires_to_add.append (aw_tmp);
					end if;

				end if;
			end query_airwire_primary;


			
			procedure remove_obsolete_primary_airwires is

				procedure query_airwire (a : in pac_airwires.cursor) is
					c : pac_airwires.cursor := airwires_primary.find (element (a));
				begin
					airwires_primary.delete (c);
				end query_airwire;
				
			begin
				airwires_to_delete.iterate (query_airwire'access);
			end remove_obsolete_primary_airwires;


			procedure assign_airwires (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				--aw : type_airwire := (type_line (make_line (1.0, 1.0, 10.0, 10.0)) with null record);
			begin
				net.route.airwires.lines := airwires_primary;
			end assign_airwires;

			
			
		begin
			put_line ("net " & to_string (key (net_cursor)));
			
			-- get x/y of all terminals:
			points := get_terminal_positions (module_cursor, net_cursor);

			-- get x/y of all vias and append to points:
			splice_points (points, get_via_positions (net_cursor));

			-- make airwires between points
			--points.iterate (query_start_point'access);
			-- Now the container "airwires" contains the airwires of
			-- clusters. But the clusters are not connected with other clusters yet.
			

			-- make airwires between clusters
			--points.iterate (query_point'access);

			-- make primary airwires (all of them originate at the same root point):
			root := first_element (points);
			points.iterate (make_primary_airwire'access);
			
			airwires_primary.iterate (query_airwire_primary'access);

			remove_obsolete_primary_airwires;

			pac_airwires.splice (
				target		=> airwires_primary, 
				before		=> pac_airwires.no_element,
				source		=> airwires_to_add);
			
			update_element (module.nets, net_cursor, assign_airwires'access);
		end query_net;



		
	begin -- query_module

		module.nets.iterate (query_net'access);


		
	end query_module;

	
begin -- update_ratsnest

	log (text => "module " 
		& enclose_in_quotes (to_string (key (module_cursor)))
		& " updating ratsnest ...",
		level => lth);

	log_indentation_up;

	update_element (generic_modules, module_cursor, query_module'access);

	log_indentation_down;
		


end update_ratsnest;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
