

with ada.containers.doubly_linked_lists;

package et_help_doubly_linked_lists is

	generic
		with package pac_list is new ada.containers.doubly_linked_lists (<>);
		with procedure process (item : in pac_list.cursor);
	procedure iterate_all (list : in pac_list.list);

	generic
		with package pac_list is new ada.containers.doubly_linked_lists (<>);
		with procedure process (list : in pac_list.cursor; proceed : out boolean);
	procedure iterate_with_proceed (list : in pac_list.list; proceed : out boolean);
	-- Iterate all elements of List with Process or until Proceed goes False.

end et_help_doubly_linked_lists;

