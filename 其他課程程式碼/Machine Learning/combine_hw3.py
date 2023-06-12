list1 = []
list2 = []
list3 = []
list4 = []
final_ans = []
with open('pre1.csv', 'r') as f:
	content = f.read()
lines = content.split('\n')
# print(lines[0])
# print(lines[1])
# print(lines[-2])
# print(lines[-1])
for line in lines[1:-1]:
    list1.append(line.split(',')[1])

with open('pre2.csv', 'r') as f:
	content = f.read()
lines = content.split('\n')
# print(lines[0])
# print(lines[1])
# print(lines[-1])
for line in lines[1:-1]:
    list2.append(line.split(',')[1])

with open('pre3.csv', 'r') as f:
	content = f.read()
lines = content.split('\n')
# print(lines[0])
# print(lines[1])
# print(lines[-1])
for line in lines[1:-1]:
    list3.append(line.split(',')[1])

with open('pre4.csv', 'r') as f:
	content = f.read()
lines = content.split('\n')
# print(lines[0])
# print(lines[1])
# print(lines[-1])
for line in lines[1:-1]:
    list4.append(line.split(',')[1])

different = 0   
for j in range(len(list1)):
    if list1[j]==list2[j] and list2[j]==list3[j] :
        final_ans.append(list1[j])
    elif list1[j]==list2[j] and list2[j]==list4[j]:
        final_ans.append(list1[j])
    elif list1[j]==list3[j] and list3[j]==list4[j]:
        final_ans.append(list1[j])
    elif list2[j]==list3[j] and list3[j]==list4[j]:
        final_ans.append(list2[j])
        different+=1
    elif list1[j]==list2[j]:
        final_ans.append(list1[j])
    elif list1[j]==list3[j]:
        final_ans.append(list1[j])
    elif list1[j]==list4[j]:
        final_ans.append(list1[j])
    elif list2[j]==list3[j]:
        final_ans.append(list2[j])
        different+=1
    elif list2[j]==list4[j]:
        final_ans.append(list2[j])
        different+=1
    elif list3[j]==list4[j]:
        final_ans.append(list3[j])
        different+=1
    else:
        final_ans.append(list1[j])

print(different)

with open('prefinal.csv', 'w') as f:
    f.write("Id,Category\n")
    for i, y in enumerate(final_ans):
        f.write('{},{}\n'.format(i, y))
    