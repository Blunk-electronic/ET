------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              KEEPOUT                                     --
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


with ada.strings;	 			use ada.strings;

package body et_keepout is

	procedure mirror_zones (
		zones	: in out pac_keepout_contours.list;
		axis	: in type_axis_2d := Y)
	is
		result : pac_keepout_contours.list;

		procedure query_zone (c : in pac_keepout_contours.cursor) is
			zone : type_keepout_contour := element (c);
		begin
			mirror (zone, axis);
		end query_zone;
		
	begin
		zones.iterate (query_zone'access);
		zones := result;
	end mirror_zones;


	procedure rotate_zones (
		zones	: in out pac_keepout_contours.list;
		angle	: in type_rotation)
	is
		result : pac_keepout_contours.list;

		procedure query_zone (c : in pac_keepout_contours.cursor) is
			zone : type_keepout_contour := element (c);
		begin
			rotate_by (zone, angle);
		end query_zone;
		
	begin
		zones.iterate (query_zone'access);
		zones := result;
	end rotate_zones;



	procedure move_zones (
		zones	: in out pac_keepout_contours.list;
		offset	: in type_distance_relative)
	is
		result : pac_keepout_contours.list;

		procedure query_zone (c : in pac_keepout_contours.cursor) is
			zone : type_keepout_contour := element (c);
		begin
			move_by (zone, offset);
		end query_zone;
		
	begin
		zones.iterate (query_zone'access);
		zones := result;
	end move_zones;

	
	
	procedure mirror_keepout_objects (
		keepout	: in out type_keepout;
		axis	: in type_axis_2d := Y)
	is begin
		mirror_zones (keepout.zones);
		-- CS mirror_cutouts (keepout.cutouts);
	end mirror_keepout_objects;
	

	procedure rotate_keepout_objects (
		keepout	: in out type_keepout;
		angle	: in type_rotation)
	is begin
		rotate_zones (keepout.zones, angle);
		-- CS rotate_cutouts (keepout.cutouts, angle);
	end rotate_keepout_objects;


	procedure move_keepout_objects (
		keepout	: in out type_keepout;
		offset	: in type_distance_relative)
	is begin
		move_zones (keepout.zones, offset);
		-- CS move_cutouts (keepout.cutouts, offset);
	end move_keepout_objects;
	
end et_keepout;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
