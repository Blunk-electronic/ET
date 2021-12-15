------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             RATSNEST                                     --
--                                                                          --
--                              B o d y                                     --
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
--		- 
--		- 


--with ada.containers.multiway_trees;

package body et_ratsnest is
	

	function contains_airwire (
		airwires	: in pac_airwires.list;
		airwire		: in type_line)
		return boolean
	is
		result : boolean := false;
	begin
		if airwires.contains (airwire) then
			result := true;

		-- The airwire could be reversed in the container
		-- (means start and end point swapped):
		elsif airwires.contains (type_line (reverse_line (airwire))) then
			result := true;

		else
			result := false;
		end if;

		return result;
	end contains_airwire;
	
	


	
	procedure iterate (
		airwires	: in pac_airwires.list;
		process		: not null access procedure (position : in pac_airwires.cursor);
		proceed		: not null access boolean)
	is
		use pac_airwires;
		c : pac_airwires.cursor := airwires.first;
	begin
		while c /= no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;




	--airwires_primary : pac_airwires.list;
	--airwires_to_add : pac_airwires.list;
	--airwires_to_delete : pac_airwires.list;


	-- The common start point of all primary airwires:
	--root : type_point;
	
	--procedure make_primary_airwire (p : in pac_points.cursor) is
	--begin
		----if p /= points.first then
		--if element (p) /= root then
			--airwires_primary.append (type_line (make_line (root, element (p))));
		--end if;
	--end make_primary_airwire;


	--type type_nearest (connected : boolean) -- via secondary airwire
	--is record
		--case connected is
			--when TRUE => null;
			--when FALSE => cursor : pac_airwires.cursor := pac_airwires.no_element;
		--end case;
	--end record;
	
	
	--function get_nearest_primary_airwire (
		--a_in	: in pac_airwires.cursor)
		--return type_nearest
	--is 
		--smallest_distance : type_distance_positive := type_distance'last;

		--aw_tmp : type_line;

		---- the elements of the return:
		--connected : boolean := false;
		--nearest_airwire : pac_airwires.cursor := pac_airwires.no_element;

		----proceed : aliased boolean := true;
		--something_found : boolean := false;

		--length_min : type_distance_positive := zero;
		----d_min : type_distance_positive := zero;

		
		--procedure query_end_point (ca : in pac_airwires.cursor) is
			--d_tmp : type_distance_positive;
		--begin
			---- Query the end point of all primary airwires except the 
			---- given airwire indicted by cursor a_in:
			--if a_in /= ca then

				---- Get the distance from end point of a_in to the end point of the
				---- candidate airwire indicated by cursor ca:
				--d_tmp := get_absolute (get_distance (element (a_in).end_point, element (ca).end_point));
				----put_line (" end " & to_string (element (cp)) & " d " & to_string (d));

				---- The distance between the ends must be shorter than 
				---- the longest of the two primary airwires:
				--if d_tmp < get_greatest_length (element (a_in), element (ca)) then

					--something_found := true;
					
					---- Update smallest_distance if current distance (d_tmp) is
					---- smaller than the old smallest_distance:
					--if d_tmp > length_min and d_tmp < smallest_distance then
						--smallest_distance := d_tmp;
						--nearest_airwire := ca;
					--end if;
					
				--end if;
			--end if;
		--end query_end_point;


	--begin
		--airwires_primary.iterate (query_end_point'access);

		--if something_found then
			
			--aw_tmp := type_line (make_line (element (a_in).end_point, element (nearest_airwire).end_point));

			--if contains_airwire (airwires_to_add, aw_tmp) then
				
				--length_min := get_length (aw_tmp);
				--smallest_distance := type_distance_positive'last;
				--something_found := false;
				--airwires_primary.iterate (query_end_point'access);

				--if something_found then
					--if airwires_to_delete.contains (element (nearest_airwire)) then
						--connected := true;
					--end if;
				--else
					--connected := true;
				--end if;
				
			--else
				----return (connected => false, cursor => nearest_airwire);
				----cursor := nearest_airwire;
				--null;
			--end if;

		--else
			----return (connected => false, cursor => pac_airwires.no_element);
			--nearest_airwire := pac_airwires.no_element;
		--end if;

		
		--if connected then
			--return (connected => true);
		--else
			--return (connected => false, cursor => nearest_airwire);
		--end if;
	--end get_nearest_primary_airwire;
	

	
	--procedure query_airwire_primary (a : in pac_airwires.cursor) is
		--aw_tmp : type_line;
	--begin
		--put_line ("prim" & to_string (element (a)));
		
		--declare 
			--nearest : type_nearest := get_nearest_primary_airwire (a);
		--begin

			--if nearest.connected then

				--put_line ("is connected");
				
				---- remove obsolete primary airwire:
				--aw_tmp := element (a);
				--if not airwires_to_delete.contains (aw_tmp) then
					--airwires_to_delete.append (aw_tmp);
				--end if;
				
			--else

				--if nearest.cursor = pac_airwires.no_element then
					--null; -- keep primary airwire
				--else
					---- near primary airwire found
					--put_line ("nearest " & to_string (element (nearest.cursor)));

					---- make secondary airwire:
					--aw_tmp := type_line (make_line (element (a).end_point, element (nearest.cursor).end_point));

					--airwires_to_add.append (aw_tmp);

					---- if required, remove obsolete primary airwire:
					--aw_tmp := (type_line (get_longest (element (a), element (nearest.cursor))));
					--if not airwires_to_delete.contains (aw_tmp) then
						--airwires_to_delete.append (aw_tmp);
					--end if;
				--end if;
				
			--end if;
		--end;
	--end query_airwire_primary;


	
	--procedure remove_obsolete_primary_airwires is

		--procedure query_airwire (a : in pac_airwires.cursor) is
			--c : pac_airwires.cursor := airwires_primary.find (element (a));
		--begin
			--airwires_primary.delete (c);
		--end query_airwire;
		
	--begin
		--airwires_to_delete.iterate (query_airwire'access);
	--end remove_obsolete_primary_airwires;


	-- make airwires between points
	--points.iterate (query_start_point'access);
	-- Now the container "airwires" contains the airwires of
	-- clusters. But the clusters are not connected with other clusters yet.
	

	-- make airwires between clusters
	--points.iterate (query_point'access);

	-- make primary airwires (all of them originate at the same root point):
	--root := first_element (points);
	--points.iterate (make_primary_airwire'access);
	
	--airwires_primary.iterate (query_airwire_primary'access);

	--remove_obsolete_primary_airwires;

	--pac_airwires.splice (
		--target		=> airwires_primary, 
		--before		=> pac_airwires.no_element,
		--source		=> airwires_to_add);

	
	function make_airwires (
		nodes	: in pac_points.list)
		return pac_airwires.list
	is
		result : pac_airwires.list;
	begin


		return result;
	end make_airwires;
	


end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
