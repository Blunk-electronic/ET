------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              KEEPOUT                                     --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
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


package body et_keepout is


	function is_circular (
		zone	: in pac_keepout_zones.cursor)
		return boolean
	is begin
		if is_circular (element (zone)) then
			return true;
		else
			return false;
		end if;
	end is_circular;


	

	procedure iterate (
		zones	: in pac_keepout_zones.list;
		process	: not null access procedure (position : in pac_keepout_zones.cursor);
		proceed	: not null access boolean)
	is
		c : pac_keepout_zones.cursor := zones.first;
	begin
		while c /= pac_keepout_zones.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
	procedure mirror_zones (
		zones	: in out pac_keepout_zones.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_keepout_zones.list;

		procedure query_zone (c : in pac_keepout_zones.cursor) is
			zone : type_keepout_zone := element (c);
		begin
			mirror (zone, axis);
			result.append (zone);
		end query_zone;
		
	begin
		zones.iterate (query_zone'access);
		zones := result;
	end mirror_zones;


	procedure rotate_zones (
		zones	: in out pac_keepout_zones.list;
		angle	: in type_rotation_model)
	is
		result : pac_keepout_zones.list;

		procedure query_zone (c : in pac_keepout_zones.cursor) is
			zone : type_keepout_zone := element (c);
		begin	
			rotate_by (zone, angle);
			result.append (zone);
		end query_zone;
		
	begin
		zones.iterate (query_zone'access);
		zones := result;
	end rotate_zones;



	procedure move_zones (
		zones	: in out pac_keepout_zones.list;
		offset	: in type_vector_model)
	is
		result : pac_keepout_zones.list;

		procedure query_zone (c : in pac_keepout_zones.cursor) is
			zone : type_keepout_zone := element (c);
		begin
			move_by (zone, offset);
			result.append (zone);
		end query_zone;
		
	begin
		zones.iterate (query_zone'access);
		zones := result;
	end move_zones;

	
	
	procedure mirror_keepout_objects (
		keepout	: in out type_keepout;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is begin
		mirror_zones (keepout.zones);
		-- CS mirror_cutouts (keepout.cutouts);
	end mirror_keepout_objects;
	

	procedure rotate_keepout_objects (
		keepout	: in out type_keepout;
		angle	: in type_rotation_model)
	is begin
		rotate_zones (keepout.zones, angle);
		-- CS rotate_cutouts (keepout.cutouts, angle);
	end rotate_keepout_objects;


	procedure move_keepout_objects (
		keepout	: in out type_keepout;
		offset	: in type_vector_model)
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
