
create table genelist(
    Hugo_Symbol text,
    Entrez_Gene_Id text,
    geneid INTEGER primary key autoincrement
);
create table genevalues(PATIENT_ID text, geneid INTEGER, genvalue INTEGER);

