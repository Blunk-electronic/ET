------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--              B O U N D E D  S T R I N G  H E L P E R S                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2026 Jesper Quorning                                       --
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

--   For correct displaying set tab width in your editor to 4.

package body et_bounded_string_helpers is

	-----------------
	-- from_string --
	-----------------

	function from_string (item : in string) return to_type is
	begin
		return to_type (bounded.to_bounded_string (item));
	end from_string;

	---------------
	-- to_string --
	---------------

	function to_string (item : in from_type) return string is
	begin
		return bounded.to_string (bounded.bounded_string (item));
	end to_string;

end et_bounded_string_helpers;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
