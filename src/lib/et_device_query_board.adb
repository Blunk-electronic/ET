------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICE QUERY OPERATIONS IN BOARD                       --
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

with et_contour_to_polygon;


package body et_device_query_board is


	function get_conductor_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		offset : constant type_distance_relative := to_distance_relative (device.position.place);

		terminals	: pac_contour_list.list;
		
		conductors	: type_conductor_objects; -- non-electrical
		conductor_polygons : pac_polygon_list.list;

		use et_contour_to_polygon;
	begin
		-- TERMINALS:
		if device.flipped = NO then
			terminals := get_terminal_contours (packge, layer_category);
			rotate_contours (terminals, device.position.rotation);
		else
			terminals := get_terminal_contours (packge, invert_category (layer_category));
			mirror_contours (terminals);
			rotate_contours (terminals, - device.position.rotation);
		end if;

		move_contours (terminals, offset);
		
		result := to_polygons (
			contours	=> terminals,
			tolerance	=> fill_tolerance,
			mode		=> EXPAND,
			debug		=> false);


		-- CONDUCTOR OBJECTS (lines, arcs, circles, texts)
		if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
			if device.flipped = NO then
				conductors := get_conductor_objects (packge, layer_category);
				rotate_conductor_objects (conductors, + device.position.rotation);
			else
				conductors := get_conductor_objects (packge, invert_category (layer_category));
				mirror_conductor_objects (conductors);
				rotate_conductor_objects (conductors, - device.position.rotation);
			end if;

			move_conductor_objects (conductors, offset);

			-- convert conductor objects to polygons:
			conductor_polygons := to_polygons (conductors, fill_tolerance);
		end if;
		
		result.splice (before => pac_polygon_list.no_element, source => conductor_polygons);

		return result;
	end get_conductor_polygons;
	

	
	function get_route_restrict_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		restrict : et_route_restrict.packages.type_one_side;
	begin
		if layer_category /= INNER then -- route restrict objects exist in outer layers only
			if device.flipped = NO then
				restrict := get_route_restrict_objects (packge, layer_category);
				rotate_route_restrict_objects (restrict, + device.position.rotation);
			else
				restrict := get_route_restrict_objects (packge, invert_category (layer_category));
				mirror_route_restrict_objects (restrict);
				rotate_route_restrict_objects (restrict, - device.position.rotation);
			end if;

			move_route_restrict_objects (restrict, to_distance_relative (device.position.place));

			-- convert restrict objects to polygons:
			result := to_polygons (restrict, fill_tolerance);
		end if;

		return result;
	end get_route_restrict_polygons;
	

	
	function get_keepout_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_keepout
	is
		use et_symbols;
		result : type_keepout;

		device : type_device_sch renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device_cursor);

		rotation : type_rotation renames device.position.rotation;
	begin
		if device.appearance = PCB then
			case face is
				when TOP =>
					if device.flipped = NO then
						result := get_keepout_objects (packge, TOP);
						rotate_keepout_objects (result, + rotation);
					else
						result := get_keepout_objects (packge, BOTTOM);
						mirror_keepout_objects (result);
						rotate_keepout_objects (result, - rotation);
					end if;

				when BOTTOM =>
					if device.flipped = NO then
						result := get_keepout_objects (packge, BOTTOM);
						rotate_keepout_objects (result, + rotation);
					else
						result := get_keepout_objects (packge, TOP);
						mirror_keepout_objects (result);
						rotate_keepout_objects (result, - rotation);
					end if;
			end case;
		end if;
		
		move_keepout_objects (result, to_distance_relative (device.position.place));
		return result;
	end get_keepout_objects;


	function get_keepout_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_keepout
	is
		result : type_keepout;

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation renames device.position.rotation;
	begin
		case face is
			when TOP =>
				if device.flipped = NO then
					result := get_keepout_objects (packge, TOP);
					rotate_keepout_objects (result, + rotation);
				else
					result := get_keepout_objects (packge, BOTTOM);
					mirror_keepout_objects (result);
					rotate_keepout_objects (result, - rotation);
				end if;

			when BOTTOM =>
				if device.flipped = NO then
					result := get_keepout_objects (packge, BOTTOM);
					rotate_keepout_objects (result, + rotation);
				else
					result := get_keepout_objects (packge, TOP);
					mirror_keepout_objects (result);
					rotate_keepout_objects (result, - rotation);
				end if;
		end case;

		move_keepout_objects (result, to_distance_relative (device.position.place));
		
		return result;
	end get_keepout_objects;



	function get_holes (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_holes.list
	is
		holes : pac_holes.list; -- to be returned
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation renames device.position.rotation;
	begin
		holes := get_hole_contours (packge);
				
		if device.flipped = YES then
			mirror_holes (holes);
			rotate_holes (holes, - rotation);
		else
			rotate_holes (holes, + rotation);
		end if;
		
		move_holes (holes, to_distance_relative (device.position.place));
		return holes;
	end get_holes;

	
	function get_hole_polygons (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		holes : pac_holes.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		rotation : type_rotation renames device.position.rotation;
	begin
		holes := get_hole_contours (packge);
		
		if device.flipped = YES then
			mirror_holes (holes);
			rotate_holes (holes, - rotation);
		else
			rotate_holes (holes, + rotation);
		end if;
		
		move_holes (holes, to_distance_relative (device.position.place));
		
		result := to_polygons (holes, fill_tolerance);
		return result;
	end get_hole_polygons;

	
	
end et_device_query_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16