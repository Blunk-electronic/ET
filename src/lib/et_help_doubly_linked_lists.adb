

package body et_help_doubly_linked_lists is

	-----------------
	-- iterate_all --
	-----------------

	procedure iterate_all (
		list : in pac_list.list)
	is
	begin
		for item in list.iterate loop
			process (item);
		end loop;
	end iterate_all;

	--------------------------
	-- iterate_with_proceed --
	--------------------------

	procedure iterate_with_proceed (
		list	: in pac_list.list;
		proceed	: out boolean)
	is
	begin
		proceed := true;

		for item in list.iterate loop
			process (item, proceed);
			exit when not proceed;
		end loop;
	end iterate_with_proceed;

end et_help_doubly_linked_lists;

